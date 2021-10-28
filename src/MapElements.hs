module MapElements where
import Parser2

data MapElements = MapElements {
                bombs :: [[Int]],
                bombermans :: [[Int]],
                bricks :: [[Int]],
                gates :: [[Int]],
                ghosts :: [[Int]],
                wall :: [[Int]]}
                deriving (Show, Read)

emptyMapElements = MapElements [] [] [] [] [] []

-- | All record fields from "data MapElements", which should be saved in the map.
staticMEfuns :: [(MapElements -> [[Int]], String)]
staticMEfuns = [
         (bricks, bricksSym),
         (gates, gatesSym),
         (ghosts, ghostsSym ),
         (wall, wallSym)]

-- | All record fields from "data MapElements", which can dissapear or change location in the map.
dynamicMEfuns :: [(MapElements -> [[Int]], String)]
dynamicMEfuns = [
         (bombermans, bombermansSym),
         (bombs, bombsSym)]

createMapElements :: JsonLike -> Either String MapElements
createMapElements xs = createMapElements' (viliusFun xs) (Right emptyMapElements)


createMapElements' ::  Either String [(String, [[Int]])] -> Either String MapElements -> Either String MapElements
createMapElements' (Left error) _ = Left error
createMapElements' _ (Left error) = Left error
createMapElements' (Right []) s = s
createMapElements' (Right [(str, cords)]) (Right s)  = case str of
  "bombs" -> Right s {bombs = cords}
  "bombermans" -> Right s {bombermans = cords}
  "bricks" -> Right s {bricks = cords}
  "gates" -> Right s {gates = cords}
  "ghosts" -> Right s {ghosts = cords}
  "wall" -> Right s {wall = cords}
  _ -> Left ("Error: a new map object called \"" ++ str ++ "\" was detected. Please update MapElements and programming logic related to the new map object.")
createMapElements' (Right (x:xs)) s = createMapElements' (Right xs) (createMapElements' (Right [x]) s)

viliusFun :: JsonLike -> Either String [(String, [[Int]])]
viliusFun json = error "not implemented"

-- | Constant values for rendering.
newlineSym = "\ESC[0m\n"
defaultSym = "\ESC[0m "
bombermansSym = "\ESC[30;43m" ++ "O"
bricksSym = "\ESC[30;45m" ++ "+"
gatesSym = "\ESC[30;46m" ++ "X"
ghostsSym = "\ESC[30;41m" ++ "H"
wallSym = "\ESC[30;100m" ++  "#"
bombsSym = "\ESC[30;102m" ++  "b"

testdata :: Either String [(String, [[Int]])]
testdata = Right [("bambermans",[[1,1]]),("bricks",[[8,7],[8,3],[8,1],[6,7],[6,5],[5,8],[5,4],[3,6],[3,4],[2,3],[2,1],[1,8],[1,7],[1,6]]),("gates",[]),("ghosts",[]),("wall",[[8,8],[8,6],[8,4],[8,2],[8,0],[7,0],[6,8],[6,6],[6,4],[6,2],[6,0],[5,0],[4,8],[4,6],[4,4],[4,2],[4,0],[3,0],[2,8],[2,6],[2,4],[2,2],[2,0],[1,0],[0,8],[0,7],[0,6],[0,5],[0,4],[0,3],[0,2],[0,1],[0,0]])]
