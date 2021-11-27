module MapElements where
import GameParser (jsonToCoordinates)
import Parser3 (JsonLike)

-- | Record fields with prefix 'b' are from bomb_surrounding
data MapElements = MapElements {
                bomb :: [[Int]],
                bombermans :: [[Int]],
                bricks :: [[Int]],
                gates :: [[Int]],
                ghosts :: [[Int]],
                wall :: [[Int]],
                bGhosts :: [[Int]],
                bBricks :: [[Int]]}
                deriving (Show, Read)

emptyMapElements = MapElements [] [] [] [] [] [] [] []

getBombs = bomb

-- | All record fields from "data MapElements", which should be saved in the map.
staticMEfuns :: [(MapElements -> [[Int]], String)]
staticMEfuns = [
         (gates, gatesSym),
         (ghosts, ghostsSym ),
         (bricks, bricksSym),
         (wall, wallSym)]

-- | All record fields from "data MapElements", which can dissapear or change location in the map.
-- | Order does matter. For example, [(bombermans, bombermansSym),(bomb, bombsSym)] shows a bomb in front if the bomberman steps on it.
dynamicMEfuns :: [(MapElements -> [[Int]], String)]
dynamicMEfuns = [
         (bomb, bombsSym),
         (bombermans, bombermansSym)]


createMapElements :: JsonLike -> Either String MapElements
createMapElements xs = createMapElements' (jsonToCoordinates xs) (Right emptyMapElements)


createMapElements' ::  Either String [(String, [[Int]])] -> Either String MapElements -> Either String MapElements
createMapElements' (Left error) _ = Left error
createMapElements' _ (Left error) = Left error
createMapElements' (Right []) s = s
createMapElements' (Right [(str, cords)]) (Right s)  = case str of
  "bomb" -> Right s {bomb = cords}
  "surrounding_bombermans" -> Right s {bombermans = cords}
  "surrounding_bricks" -> Right s {bricks = cords}
  "surrounding_gates" -> Right s {gates = cords}
  "surrounding_ghosts" -> Right s {ghosts = cords}
  "surrounding_wall" -> Right s {wall = cords}
   -- Flush bomb_surrounding bombermans, gates & walls; they are useless for now
  "bomb_surrounding_bombermans" -> Right s
  "bomb_surrounding_gates" -> Right s
  "bomb_surrounding_wall" -> Right s

  "bomb_surrounding_ghosts" -> Right s {bGhosts = cords}
  "bomb_surrounding_bricks" -> Right s {bBricks = cords}
  _ -> Left ("Error: a new map object called \"" ++ str ++ "\" was detected. Please update MapElements and programming logic related to the new map object.")
createMapElements' (Right (x:xs)) s = createMapElements' (Right xs) (createMapElements' (Right [x]) s)


-- | Constant values for rendering.
newlineSym = "\ESC[0m\n"
defaultSym = "\ESC[0m "
bombermansSym = "\ESC[30;43m" ++ "O"
bricksSym = "\ESC[30;45m" ++ "+"
gatesSym = "\ESC[30;46m" ++ "X"
ghostsSym = "\ESC[30;41m" ++ "H"
wallSym = "\ESC[30;100m" ++  "#"
bombsSym = "\ESC[30;102m" ++  "b"
