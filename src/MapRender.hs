{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module MapRender where
import Data.Either
import MapElements
    ( MapElements,
      staticMEfuns,
      dynamicMEfuns,
      createMapElements,
      newlineSym,
      defaultSym, getBombs, bricksSym, ghostsSym )
import Parser3 ( JsonLike(..), runParser )
import Lib3 (InitData (..), fromJsonLike, FromJsonLike (fromJsonLike))


-- | State JsonLike InitData StaticMapData DymanicMapData ErrorMessage
type Entry = (Int, String)
type Map = [Entry]
type DynamicEntries = [Entry]
type Bombs = [[Int]]
data State = State JsonLike InitData Map DynamicEntries Bombs String
  deriving (Show)


-- | Is called in a very beginning of a game
init ::
  -- | Initial data of the game
  InitData ->
  -- | First json message before any moves performed
  JsonLike ->
  -- | An initial state based on initial data and first message
  State
init i = update (State JsonLikeNull i (generateEmptyMap i) [] [] "")

-- | Is called after every user interaction (key pressed)
update ::
  -- | Current state
  State ->
  -- | Json message from server
  JsonLike ->
  -- | A new state, probably based on the message arrived
  State
update (State json i m d b e) newJson = updateMap (State newJson i m d b e)

-- | Renders the current state
render ::
  -- | A state to be rendered
  State ->
  -- | A string which represents the state. The String is rendered from the upper left corner of terminal.
  String
render (State json iData sMap dMap lBomb "") = mapToString
  where
    mapWithDynamicEls = addToMap dMap sMap
    w = gameWidth iData
    mapToString = concat [str ++ p | (i, str) <- mapWithDynamicEls,
                          let p = if (i + 1) `mod` w == 0 then newlineSym else ""]
render (State _ _ _ _ _ errorMessage) = errorMessage


updateMap :: State -> State
updateMap (State json iData prevMap _ lBomb _) =
  if isRight mapElements then
    let mapElements' = head (rights [mapElements])
        entries = concat (getStaticEntries mapElements' iData)
        dynamicElements = concat (getDynamicEntries mapElements' iData)
        newBombs = getBombs mapElements'

        mapAfterExplosion = explodeBombs prevMap iData lBomb newBombs

        newMap = addToMap entries mapAfterExplosion
    in State json iData newMap dynamicElements newBombs""
  else
    State json iData [] [] [] (head (lefts [mapElements]))
  where
      mapElements = createMapElements json


-- | IMPORTANT: defined a type Entry = (Int, String).

-- | Takes: InitData.
-- | Returns: Map (or [(Int, String)] - same thing),
-- |   where Int - an entry number (y + x * gameWidth), String - defaultSym symbol;
-- |   there should be gameWidth * gameHeight number of entries.
-- | Example: generateEmptyMap (InitData 3 4)   ---> [(0, " "), (1, " "), ..., (11, " ")].
generateEmptyMap :: InitData -> Map
generateEmptyMap (InitData w h) = [(entryNumber, defaultSym) | entryNumber <- [0 .. (w * h)]]


-- | Takes: (es) [Entry], (gMap) [Entry], where (es) is list of surrounding entries & (gMap) is map filled with values.
-- | Returns: [Entry], a new map with values inserted from es [Entry] to gMap [Entry].
-- | Must: use addToMap' (that takes 1 entry and adds it to the map). 
addToMap :: [Entry] -> Map -> [Entry]
addToMap es gMap = foldl (flip addToMap') gMap es


-- | Takes: (n, str) Entry, (gMap) [Entry]
-- | Returns: [Entry], a gMap that has deleted n-th element and inserted into that place (n, str).
-- | Must: check if str from (n, str) is same as n-th element from gMap. If yes, then don't do costly operations.
addToMap' :: Entry -> Map -> Map
addToMap' (n, str) gMap | snd (gMap !! n) == str = gMap
                        | otherwise = take n gMap ++ [(n, str)] ++ drop (n + 1) gMap

-- | Same as addToMap' but can be used to replace specific map elements.
replaceInMap :: [String] -> Entry -> Map -> Map
replaceInMap targets (n, str) gMap | snd (gMap !! n) `elem` targets = take n gMap ++ [(n, str)] ++ drop (n + 1) gMap
                                   | otherwise = gMap

-- | Takes: MapElements, InitData.
-- | Returns: [[Entry]], where [Entry] is received by applying getEntries' for each staticMEfuns element.
-- | HINT: use function "map".
getStaticEntries :: MapElements -> InitData -> [[Entry]]
getStaticEntries surr iData = map (getEntries' surr iData) staticMEfuns

getDynamicEntries :: MapElements -> InitData -> [[Entry]]
getDynamicEntries surr iData = map (getEntries' surr iData) dynamicMEfuns

-- | Takes : (a, String), MapElements, InitData.
-- | Returns: [(Int, String)], where String is a symbol, Int - sum of coordinates (y + x * width).
getEntries' :: MapElements -> InitData -> (MapElements -> [[Int]], String) -> [Entry]
getEntries' surr iData (fun, sym) = getEntries'' iData (fun surr, sym)

getEntries'' :: InitData -> ([[Int]], String) -> [Entry]
getEntries'' iData (entries, sym) = [(p, sym) | [x, y] <- entries, let p = y + x * w]
  where
    w = gameWidth iData

-- | Takes prevMap, old bombs and new bombs. If old bombs exploded, 
explodeBombs :: [Entry] -> InitData -> [[Int]] -> [[Int]] -> [Entry]
explodeBombs gMap _ [] [] = gMap
explodeBombs gMap _ [] [[x1, y1]] = gMap
explodeBombs gMap iData [[x, y]] [] = removeExplodedBricks gMap iData x y
explodeBombs gMap iData [[x, y]] [[x1, y1]]
  | x1 == x1 && y == y1 = gMap
  | otherwise = removeExplodedBricks gMap iData x y
explodeBombs _ _ a b = error ("Application supporting multiple bombs is not implemented yet." ++ show a ++ show b)


removeExplodedBricks :: [Entry] -> InitData -> Int -> Int -> [Entry]
removeExplodedBricks gMap iData x y = foldl (flip (replaceInMap [bricksSym, ghostsSym])) gMap badBricks
  where w = gameWidth iData
        h = gameHeight iData
        badBricks = getEntries'' iData (getExplosionPoints x y w h, defaultSym)

getExplosionPoints :: Int -> Int -> Int -> Int -> [[Int]]
getExplosionPoints x y w h = filter (\[x', y'] -> x' < h && y' < w) [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1], [x, y]]