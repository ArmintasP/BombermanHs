{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib2 where
import Data.Either
import MapElements
    ( MapElements,
      staticMEfuns,
      dynamicMEfuns,
      createMapElements,
      newlineSym,
      defaultSym )
import Parser2 ( JsonLike(..), runParser )

data InitData = InitData
  { gameWidth :: Int,
    gameHeight :: Int
  }
  deriving (Show)

-- | State JsonLike InitData StaticMapData DymanicMapData ErrorMessage
data State = State JsonLike InitData [Entry] [Entry] String
  deriving (Show)

type Entry = (Int, String)

parseJsonMessage :: String -> Either String JsonLike
parseJsonMessage = runParser

-- | Is called in a very beginning of a game
init ::
  -- | Initial data of the game
  InitData ->
  -- | First json message before any moves performed
  JsonLike ->
  -- | An initial state based on initial data and first message
  State
init i = update (State JsonLikeNull i (generateEmptyMap i) [] "")

-- | Is called after every user interaction (key pressed)
update ::
  -- | Current state
  State ->
  -- | Json message from server
  JsonLike ->
  -- | A new state, probably based on the message arrived
  State
update (State json i m b e) newJson = updateMap (State newJson i m b e)

-- | Renders the current state
render ::
  -- | A state to be rendered
  State ->
  -- | A string which represents the state. The String is rendered from the upper left corner of terminal.
  String
render (State json iData sMap dMap "") = mapToString
  where
    mapWithDynamicEls = addToMap dMap sMap
    w = gameWidth iData
    mapToString = concat [str ++ p | (i, str) <- mapWithDynamicEls,
                          let p = if (i + 1) `mod` w == 0 then newlineSym else ""]
render (State _ _ _ _ errorMessage) = errorMessage

updateMap :: State -> State
updateMap (State json iData prevMap _ _) =
  if isRight mapElements then
    let mapElements' = head (rights [mapElements])
        entries = concat (getStaticEntries mapElements' iData)
        newMap = addToMap entries prevMap
        dynamicElements = concat (getDynamicEntries mapElements' iData)
    in State json iData newMap dynamicElements ""
  else
    State json iData [] [] (head (lefts [mapElements])) 
  where
      mapElements = createMapElements json


-- | IMPORTANT: defined a type Entry = (Int, String).

-- | Takes: InitData.
-- | Returns: [Entry] (or [(Int, String)] - same thing),
-- |   where Int - an entry number (y + x * gameWidth), String - defaultSym symbol;
-- |   there should be gameWidth * gameHeight number of entries.
-- | Example: generateEmptyMap (InitData 3 4)   ---> [(0, " "), (1, " "), ..., (11, " ")].
generateEmptyMap :: InitData -> [(Int, String)]
generateEmptyMap (InitData w h) = [(entryNumber, defaultSym) | entryNumber <- [0 .. (w * h)]]


-- | Takes: (es) [Entry], (gMap) [Entry], where (es) is list of surrounding entries & (gMap) is map filled with values.
-- | Returns: [Entry], a new map with values inserted from es [Entry] to gMap [Entry].
-- | Must: use addToMap' (that takes 1 entry and adds it to the map). 
addToMap :: [Entry] -> [Entry] -> [Entry]
addToMap es gMap = foldl (flip addToMap') gMap es


-- | Takes: (n, str) Entry, (gMap) [Entry]
-- | Returns: [Entry], a gMap that has deleted n-th element and inserted into that place (n, str).
-- | Must: check if str from (n, str) is same as n-th element from gMap. If yes, then don't do costly operations.
addToMap' :: (Int, String) -> [Entry] -> [Entry]
addToMap' (n, str) gMap | snd (gMap !! n) == str = gMap
                       | otherwise = take n gMap ++ [(n, str)] ++ drop (n + 1) gMap


-- | Takes: MapElements, InitData.
-- | Returns: [[Entry]], where [Entry] is received by applying getEntries' for each staticMEfuns element.
-- | HINT: use function "map".
getStaticEntries :: MapElements -> InitData -> [[Entry]]
getStaticEntries surr iData = map (getEntries' surr iData) staticMEfuns

getDynamicEntries :: MapElements -> InitData -> [[Entry]]
getDynamicEntries surr iData = map (getEntries' surr iData) dynamicMEfuns

-- | Takes : (a, String), MapElements, InitData.
-- | Returns: [(Int, String)], where String is a symbol, Int - sum of coordinates (y + x * width).
getEntries' :: MapElements -> InitData -> (MapElements -> [[Int]], String) -> [(Int, String)]
getEntries' surr iData (fun, sym) = [(p, sym) | [x, y] <- fun surr, let p = y + x * w]
  where
    w = gameWidth iData

