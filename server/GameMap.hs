module GameMap where
import Data.Either as E (Either (..))
import Lib3 (Command(..), Direction(..), ToJsonLike (toJsonLike))
import Parser4 (JsonLike(..))

newtype GameMap = GameMap [String]

type Point = (Int, Int)
type Coordinates = [Point]


data Status =  Playing | GameLost | GameWon
  deriving (Show)

data GameData = GameData {
        bombermans :: Coordinates,
        ghosts :: [(Point, Direction)],
        bomb :: Coordinates,
        bricks :: Coordinates,
        gates :: Coordinates,
        wall :: Coordinates,
        mapSize :: (Int, Int),
        status :: Status
} deriving (Show)

gameDataEmpty :: GameData
gameDataEmpty = GameData [] [] [] [] [] [] (0, 0) Playing

instance ToJsonLike GameData where
  toJsonLike gd@(GameData bm gh bo br ga wa _ _) =  E.Right $ JsonLikeObject [surrounding, bomb, bomb_surrounding]
    where
      bm' = linkedList bm
      gh' = linkedList $ getAllGhostCoordinates gd
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
  | a && b && c = (getSurrounding gd) {bomb = bomb gd }
  | a && b = (getSurrounding gd) {bomb = bomb gd }
  | a && c = gameDataEmpty {bomb = bomb gd }
  | b && c = (getSurrounding gd) {bomb = []}
  | otherwise = gd
  where a = FetchSurrounding `elem` cms
        b = FetchBombStatus `elem` cms
        c = FetchBombSurrounding `elem` cms

moveBomberman :: Direction -> GameData -> GameData
moveBomberman direction gd = case direction of
  Lib3.Up -> checkGameStatus $ moveBomberman' (x - 1, y) gd
  Lib3.Down -> checkGameStatus $ moveBomberman' (x + 1, y) gd
  Lib3.Left -> checkGameStatus $ moveBomberman' (x, y - 1) gd
  Lib3.Right -> checkGameStatus $ moveBomberman' (x, y + 1) gd
  where
    [(x, y)] = bombermans gd

-- | Not a smart moving algorithm, though.
moveGhosts :: GameData -> GameData
moveGhosts gd = checkGameStatus $ gd {ghosts = gh'}
  where
    gh = ghosts gd
    gh' =  [moveGhost x gd | x <- gh]

moveGhost :: (Point, Direction) -> GameData -> (Point, Direction)
moveGhost x gd = moveGhost' x gd 0

-- | Multiple rules for more "randomized" movement.
moveGhost' :: (Point, Direction) -> GameData -> Int -> (Point, Direction)
moveGhost' ((x, y), dir) gd count
  | count >= 6 = ((x, y), dir)
  | count == 1 = case dir of
      Lib3.Left -> moveLeft Lib3.Right
      Lib3.Right -> moveRight Lib3.Left
      Lib3.Up -> moveUp Lib3.Down
      Lib3.Down -> moveDown Lib3.Up
  | otherwise = case dir of
      Lib3.Left -> moveLeft Lib3.Up
      Lib3.Right -> moveRight Lib3.Down
      Lib3.Up -> moveUp Lib3.Right
      Lib3.Down -> moveDown Lib3.Left
  where
    isValid p = not $ (p `elem` (wall gd ++ bricks gd))
    count' = count + 1
    moveLeft alt = if isValid (x, y - 1) then ((x, y - 1), dir) else moveGhost' ((x, y), alt) gd count'
    moveRight alt =  if isValid (x, y + 1) then ((x, y + 1), dir) else moveGhost' ((x, y), alt) gd count'
    moveUp alt = if isValid (x - 1, y) then ((x - 1, y), dir) else moveGhost' ((x, y), alt) gd count'
    moveDown alt = if isValid (x + 1, y) then ((x + 1, y), dir) else moveGhost' ((x, y), alt) gd count'


checkGameStatus :: GameData -> GameData
checkGameStatus gd
  | isLost = gd {status = GameLost}
  | isWon = gd {status = GameWon}
  | otherwise = gd
  where [bm] = bombermans gd
        ga = gates gd
        gh = getAllGhostCoordinates gd
        isWon = bm `elem` ga
        isLost = bm `elem` gh

--   |
--  —b—  Bomb's explosion spans to each direction (up, down, left and right) with radius of 1.
--   |
explodeBomb :: GameData -> GameData
explodeBomb gd = gd {bricks = remainingBricks, ghosts = remainingGhosts, bomb = []}
  where
    [(x, y)] = bomb gd
    blastSpots = [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    explode x = removePoints x blastSpots
    remainingBricks = explode (bricks gd)
    remainingGhosts = [(p', dir) | (p, dir) <- ghosts gd, p' <- explode [p], length p' > 0]

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
    getInRange' x = getInRange radius bm x
    br = getInRange' $ bricks gd
    gh = [(p', dir) | (p, dir) <- ghosts gd, p' <- getInRange' [p], length p' > 0]
    gt = getInRange' $ gates gd
    wl = getInRange' $ wall gd

-- | Removes points that matches the blacklist.
removePoints :: [Point] -> [Point] -> [Point]
removePoints points blacklist = [p | p <- points, p `notElem` blacklist]

getAllGhostCoordinates :: GameData -> Coordinates
getAllGhostCoordinates (GameData _ gh _ _ _ _ _ _) = [p | (p, _) <- gh]

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
gameMapCollection = filter isCorrectMap' gameMapCollection'

-- Make sure to check if your map is corret with function isCorrectMap before adding it to the collection,
-- otherwise it won't be inluded when server is launched.
gameMapCollection' :: [GameMap]
gameMapCollection' = [gameMap1, gameMap2, gameMap3]



isCorrectMap :: GameMap -> Either String Bool
isCorrectMap (GameMap gm)
  | not lengthCheck = E.Left "Error: Map length is not the same in all lines."
  | bm /= 1 = E.Left "Error: There must be one and only one bomberman in the map."
  | ga < 1 = E.Left "Error: There must be at least one gate in the map."
  | otherwise = E.Right True
  where lengthCheck = isCorrectLength gm
        (bm, ga) = countGatesAndBombermans (concat gm) (0, 0)

gameMap1 :: GameMap
gameMap1 = GameMap [
        "XXXXXXXXXXXXXXX",
        "XM    BBBBBBBBX",
        "XBXBX X X X X X",
        "X G B B   B   X",
        "X X X X X XBXBX",
        "X G B   B     X",
        "X X XBXBX XBXBX",
        "X G  G    B B X",
        "XBXBX XBXBXBXBX",
        "X     BG      X",
        "XBXBX XBXBXBX X",
        "X     B       X",
        "X XBXBXBXBXBXBX",
        "X  BG      B OX",
        "XXXXXXXXXXXXXXX"]

gameMap2 :: GameMap
gameMap2 = GameMap [
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
        "X     BBBBBBBBX                                           OX",
        "XG    X X X X X                                            X",
        "X    GB       X                                            X",
        "X M   X X XBXBX                                            X",
        "X   B   B     X                                            X",
        "X X XBXBX XBXBX          BBBBBBBBBBBBBB                    X",
        "X  G      B B X          B      G     B                    X",
        "XBXBX XBXBXBXBX          BBBBBBBBBBBBBB                    X",
        "X             X                                            X",
        "XBXBXBXBXBXBX X                                            X",
        "X             X                                            X",
        "X XBXBXBXBXBXBX                                            X",
        "X                                                          X",
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"]

gameMap3 :: GameMap
gameMap3 = GameMap [
        "XXXXXXXXXXXX",
        "XG B M B   X",
        "XBBBBBB G  X",
        "X         BX",
        "X        BOX",
        "XXXXXXXXXXXX"]

bombermansSym = 'M'
bricksSym = 'B'
gatesSym = 'O'
ghostsSym = 'G'
wallSym = 'X'

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
    (maxY, maxX) = mapSize gd
    isOutOfMargins = x < 0 || y < 0 || x >= maxX || y >= maxY
    isBlocked = ((x, y) `elem` bricks gd) || ((x, y) `elem` wall gd)


getInRange :: Int -> Point -> Coordinates -> Coordinates
getInRange r p [] = []
getInRange r (x', y') [(x,y)]
  | (x' - r <= x) && (x' + r >= x) && (y' - r <= y) && (y' + r >= y) = [(x, y)]
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
  | c == ghostsSym = gd {ghosts = ghosts gd ++ [(cord, Lib3.Left)]}
  | c == bricksSym = gd {bricks = bricks gd ++ [cord]}
  | c == gatesSym = gd {gates = gates gd ++ [cord]}
  | c == wallSym = gd {wall = wall gd ++ [cord]}
  | otherwise = gd


isCorrectLength :: [String] -> Bool
isCorrectLength gm = head ([False | (l1, l2) <- zip (Prelude.init gm) (tail gm), length l1 /= length l2] ++ [True])

countGatesAndBombermans :: String -> (Int, Int) -> (Int, Int)
countGatesAndBombermans [] (bm, ga) = (bm, ga)
countGatesAndBombermans (x:xs) (bm, ga)
  | x == bombermansSym = countGatesAndBombermans xs (bm + 1, ga)
  | x == gatesSym = countGatesAndBombermans xs (bm ,ga + 1)
  | otherwise = countGatesAndBombermans xs (bm, ga)


isCorrectMap' :: GameMap -> Bool
isCorrectMap' gm = case isCorrectMap gm of
  E.Left _ ->  False
  E.Right _ -> True
