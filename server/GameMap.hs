module GameMap where
import Data.Either as E (Either (..))
import Lib3 (Command(..), Direction(..), ToJsonLike (toJsonLike))
import Parser3 (JsonLike(..))
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M

newtype GameMap = GameMap [String]
type Point = (Int, Int)
type Coordinates = [Point]
type Games = M.Map String GameData

data GameData = GameData {
        bombermans :: Coordinates,
        ghosts :: Coordinates,
        bomb :: Coordinates,
        bricks :: Coordinates,
        gates :: Coordinates,
        wall :: Coordinates,
        mapSize :: (Int, Int)
} deriving (Show)

gameDataEmpty :: GameData
gameDataEmpty = GameData [] [] [] [] [] [] (0, 0)

instance ToJsonLike GameData where
  toJsonLike (GameData bm gh bo br ga wa _) =  E.Right $ JsonLikeObject [surrounding, bomb, bomb_surrounding]
    where
      bm' = linkedList bm
      gh' = linkedList gh
      bo' = linkedList bo
      br' = linkedList br
      ga' = linkedList ga
      wa' = linkedList wa
      surrounding = ("surrounding", JsonLikeObject [
        ("bombermans", bm'), ("bricks", br'), ("ghosts", gh'), ("gates", ga'), ("wall", wa')])
      bomb = ("bomb", bo')
      bomb_surrounding = ("bomb_surrounding", JsonLikeNull )

-- | Modifies current map, unless it's a fetch command - then it does nothing.
applyCommand :: Command -> (GameData, Bool) -> (GameData, Bool)
applyCommand c (gd, b) = case c of
  MoveBomberman direction -> (moveBomberman direction gd, b)
  PlantBomb -> plantBomb gd
  FetchSurrounding -> (gd, b)
  FetchBombStatus  -> (gd, b)
  FetchBombSurrounding  -> (gd, b)

-- | Use this function for fetch commands.
applyFetchCommands :: GameData -> [Command] -> GameData
applyFetchCommands gd cms 
  | a && b && c = gd
  | a && b = (getSurrounding gd) {bomb = bomb gd }
  | a && c = gameDataEmpty {bomb = bomb gd }
  | b && c = (getSurrounding gd) {bomb = []}
  | otherwise = gd
  where a = FetchSurrounding `elem` cms
        b = FetchBombStatus `elem` cms
        c = FetchBombSurrounding `elem` cms

moveBomberman :: Direction -> GameData -> GameData
moveBomberman direction gd = case direction of
  Lib3.Left -> moveBomberman' (x - 1, y) gd
  Lib3.Right -> moveBomberman' (x + 1, y) gd
  Lib3.Up -> moveBomberman' (x, y - 1) gd
  Lib3.Down -> moveBomberman' (x, y + 1) gd
  where
    [(x, y)] = bombermans gd

--   |
--  —b—  Bomb's explosion spans to each direction (up, down, left and right) with radius of 1.
--   |
explodeBomb :: GameData -> GameData
explodeBomb gd = gd {bricks = remainingBricks, ghosts = remainingGhosts, bomb = []}
  where
    [(x, y)] = bomb gd
    blastSpots = [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    remainingBricks = removePoints (bricks gd) blastSpots
    remainingGhosts = removePoints (ghosts gd) blastSpots

plantBomb :: GameData -> (GameData, Bool)
plantBomb gd = if isBombPlanted then (gd, False) else (gd {bomb = plantSpot}, True)
  where
    plantSpot = bombermans gd
    bomb' = bomb gd
    isBombPlanted = length bomb' == 1

-- | If there is a bomb (x, y), return GameData with elements that are near it (blastSpots). Otherwise return empty GameData.
getBombSurrounding :: GameData -> GameData
getBombSurrounding = error "Not implemented"

-- | Fetch function. Returns bomb status only.
getBombStatus :: GameData -> GameData
getBombStatus gd = gd {bomb = bomb gd}

-- | Bomberman's vision range
radius :: Int
radius = 5

-- | Fetch function. Returns only surrounding objects.
getSurrounding :: GameData -> GameData
getSurrounding gd = gd {bombermans = [bm], ghosts = gh, bomb = [], bricks = br, gates = gt, wall = wl}
  where
    [bm] = bombermans gd
    br = getInRange radius bm $ bricks gd
    gh = getInRange radius bm $ ghosts gd
    gt = getInRange radius bm $ gates gd
    wl = getInRange radius bm $ wall gd

-- | Removes points that matches the blacklist.
removePoints :: [Point] -> [Point] -> [Point]
removePoints points blacklist = [p | p <- points, p `notElem` blacklist]

getGameMapData :: GameMap -> GameData
getGameMapData x = constructGameData gd gameMapElements
  where
    gameMapElements = getGameMapData' x
    gd = gameDataEmpty {mapSize = getGameMapSize x}


getGameMapSize :: GameMap -> (Int, Int)
getGameMapSize x = (getGameMapWidth x, getGameMapHeight x)

getGameMapWidth :: GameMap -> Int
getGameMapWidth (GameMap gameMap) = length $ head gameMap

getGameMapHeight :: GameMap -> Int
getGameMapHeight (GameMap gameMap) = length gameMap

gameMapCollection :: [GameMap]
gameMapCollection = [gameMap1, gameMap2]

gameMap1 :: GameMap
gameMap1 = GameMap [
        "XXXXXXXXXXXXXXX",
        "XM    BBBBBBBBX",
        "XBXBX X X X X X",
        "X   B B   B   X",
        "X X X X X XBXBX",
        "X   B   B     X",
        "X X XBXBX XBXBX",
        "X         B B X",
        "XBXBX XBXBXBXBX",
        "X     BG      X",
        "XBXBX XBXBXBX X",
        "X     B       X",
        "X XBXBXBXBXBXBX",
        "X            OX",
        "XXXXXXXXXXXXXXX"];

gameMap2 :: GameMap
gameMap2 = GameMap [
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
        "XM    BBBBBBBBX                                           OX",
        "X     X X X X X                                            X",
        "X     Bb      X                                            X",
        "X     X X XBXBX                                            X",
        "X   B   B     X                                            X",
        "X X XBXBX XBXBX          BBBBBBBBBBBBBB                    X",
        "X         B B X          B      G     B                    X",
        "XBXBX XBXBXBXBX          BBBBBBBBBBBBBB                    X",
        "X             X                                            X",
        "XBXBXBXBXBXBX X                                            X",
        "X             X                                            X",
        "X XBXBXBXBXBXBX                                            X",
        "X                                                          X",
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"];

bombermansSym = 'M'
bricksSym = 'B'
gatesSym = 'O'
ghostsSym = 'G'
wallSym = 'X'
bombsSym = 'b'

-- |
-- | Helper functions
-- |



linkedList :: Coordinates -> JsonLike
linkedList [] = JsonLikeObject [("head", JsonLikeNull), ("tail", JsonLikeNull)]
linkedList ((x, y):xs) = JsonLikeObject [
  ("head", JsonLikeList [JsonLikeInteger $ toInteger x, JsonLikeInteger $ toInteger y]),
  ("tail", linkedList xs)]


moveBomberman' :: Point -> GameData -> GameData
moveBomberman' (x, y) gd = if isOutOfMargins || isBlocked then gd else gd {bombermans = [(x, y)]}
  where
    (maxX, maxY) = mapSize gd
    isOutOfMargins = x < 0 || y < 0 || x >= maxX || y >= maxY
    isBlocked = ((x, y) `elem` bricks gd) || ((x, y) `elem` wall gd)


getInRange :: Int -> Point -> Coordinates -> Coordinates
getInRange r p [] = []
getInRange r (x', y') [(x,y)]
  | (x' - r <= x) && (x' + r >= x) && (y' - r <= x) && (y' + r >= y) = [(x, y)]
  | otherwise = []
getInRange r p (x:xs) = getInRange r p [x] ++ getInRange r p xs


constructGameData :: GameData -> [(Char, Point)] -> GameData
constructGameData gd [] = gd
constructGameData gd [(ch, point)] = addPoint ch point gd
constructGameData gd ((ch, point):xs) = constructGameData (addPoint ch point gd) xs

getGameMapData' :: GameMap -> [(Char, Point)]
getGameMapData' (GameMap m) = do
  (str, y) <- zip m [0..]
  (sym, x) <- zip str [0..]
  return (sym, (y, x))


addPoint :: Char -> Point -> GameData -> GameData
addPoint c cord gd
  | c == bombermansSym = gd {bombermans = bombermans gd ++[cord]}
  | c == ghostsSym = gd {ghosts = ghosts gd ++ [cord]}
  | c == bricksSym = gd {bricks = bricks gd ++ [cord]}
  | c == gatesSym = gd {gates = gates gd ++ [cord]}
  | c == wallSym = gd {wall = wall gd ++ [cord]}
  | c == bombsSym = gd {bomb = [cord]}
  | otherwise = gd