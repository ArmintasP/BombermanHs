module Lib1 where

import GHC.StableName (StableName)
import Parser

data InitData = InitData
  { gameWidth :: Int,
    gameHeight :: Int
  }
  deriving (Show)

-- | Change State the way need but please keep
--  the name of the type, i.e. "State"
-- | State String InitData Map BombermansEntries originalJsonString
data State = State String InitData [Entry] [Entry] String
  deriving (Show)


type Entry = (Int, String)

-- | Is called in a very beginning of a game
init ::
  -- | Initial data of the game
  InitData ->
  -- | First json message before any moves performed
  String ->
  -- | An initial state based on initial data and first message
  State
init i j = update (State "" i (generateEmptyMap i) [] j ) j

-- | Is called after every user interaction (key pressed)
update ::
  -- | Current state
  State ->
  -- | Json message from server
  String ->
  -- | A new state, probably based on the message arrived
  State
update (State str i m b db) newStr | str == newStr = State str i m b newStr
                                   | otherwise = updateMap (State newStr i m b newStr)

-- | Renders the current state
render ::
  -- | A state to be rendered
  State ->
  -- | A string which represents the state. The String is rendered from the upper left corner of terminal.
  String
render (State _ iData gMap bMan db) = mapToString ++ db
  where
    mapWithBMan = inserEntriesToMap bMan gMap
    w = gameWidth iData
    mapToString = concat [str ++ p | (i, str) <- mapWithBMan, let p = if (i + 1) `mod` w == 0 then newlineSym else ""]


updateMap :: State -> State
updateMap (State str iData prevMap _ db) = State str iData newMap bMans db
  where
    surr = getSurrounding str
    entries = concat (getSurroundingEntries surr iData)
    newMap = inserEntriesToMap entries prevMap
    bMans = getEntries surr iData (bombermans, bombermansSym)


-- IMPORTANT: defined a type Entry = (Int, String).

-- Takes: InitData.
-- Returns: [Entry] (or [(Int, String)] - same thing),
--     where Int - an entry number (y + x * gameWidth), String - defaultSym symbol;
--     there should be gameWidth * gameHeight number of entries.
-- Example: generateEmptyMap (InitData 3 4)   ---> [(0, " "), (1, " "), ..., (11, " ")].
generateEmptyMap :: InitData -> [(Int, String)]
generateEmptyMap (InitData w h) = [(entryNumber, defaultSym) | entryNumber <- [0 .. (w * h)]]

-- | Can be deleted once function is implemented.
dummyValue2 = [(0, bricksSym), (1, defaultSym), (2, gatesSym)]

-- | Takes: (es) [Entry], (gMap) [Entry], where (es) is list of surrounding entries & (gMap) is map filled with values.
-- | Returns: [Entry], a new map with values inserted from es [Entry] to gMap [Entry].
-- | Must: use addToMap (that takes 1 entry and adds it to the map). 
inserEntriesToMap :: [Entry] -> [Entry] -> [Entry]
inserEntriesToMap es gMap = dummyValue2

-- | Takes: (n, str) Entry, (gMap) [Entry]
-- | Returns: [Entry], a gMap that has deleted n-th element and inserted into that place (n, str).
-- | Must: check if str from (n, str) is same as n-th element from gMap. If yes, then don't do costly operations.
addToMap :: (Int, String) -> [Entry] -> [Entry]
addToMap (n, str) gMap = dummyValue2

-- | Takes: Surrounding, InitData.
-- | Returns: [[Entry]], where [Entry] is received by applying getEntries for each surrFuns element.
-- | HINT: use function "map".
getSurroundingEntries :: Surrounding -> InitData -> [[Entry]]
getSurroundingEntries surr iData = map (getEntries surr iData) surrFuns


-- | Takes : (a, String), Surrounding, InitData.
-- | Returns: [(Int, String)], where String is a symbol, Int - sum of coordinates (y + x * width).
getEntries :: Surrounding -> InitData -> (Surrounding -> [[Int]], String) -> [(Int, String)]
getEntries surr iData (fun, sym) = [(p, sym) | [x, y] <- fun surr, let p = y + x * w]
  where
    w = gameWidth iData


-- | All record fields from "data Surrounding", but bombermans is omitted! Do not add it.
surrFuns :: [(Surrounding -> [[Int]], String)]
surrFuns = [
         (bricks, bricksSym),
         (gates, gatesSym),
         (ghosts, ghostsSym ),
         (wall, wallSym)]


-- | Don't bother with colors for now.
newlineSym = "\n"--"\ESC[0m\n"
defaultSym = " "--"\ESC[0m "
bombermansSym = "O" --"\ESC[30;43m" ++ "O"
bricksSym = "B" --"\ESC[30;45m" ++ "+"
gatesSym = "X" --"\ESC[30;46m" ++ "X"
ghostsSym = "H" --"\ESC[30;41m" ++ "H"
wallSym = "#" --"\ESC[30;100m" ++  "#"