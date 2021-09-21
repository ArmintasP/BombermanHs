module Parser where

-- | For testing purposes
testStr :: String
testStr = "{\"surrounding\":{\"bombermans\":[[1,1]],\"bricks\":[[1,6],[1,7],[1,8],[2,1],[2,3],[3,4],[3,6],[5,4],[5,8],[6,5],[6,7],[8,1],[8,3],[8,7]],\"gates\":[],\"ghosts\":[],\"wall\":[[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[0,7],[0,8],[1,0],[2,0],[2,2],[2,4],[2,6],[2,8],[3,0],[4,0],[4,2],[4,4],[4,6],[4,8],[5,0],[6,0],[6,2],[6,4],[6,6],[6,8],[7,0],[8,0],[8,2],[8,4],[8,6],[8,8]]}}"

-- | In REPL: x' ParsedSurr should give [[Int]], where x' is
-- | a record type, i. e., bombermans, bricks.
parsedSurr :: Surrounding
parsedSurr = getSurrounding testStr

data Surrounding = Surrounding {
                bombermans :: [[Int]],
                bricks :: [[Int]],
                gates :: [[Int]],
                ghosts :: [[Int]],
                wall :: [[Int]]}
                deriving (Show, Read)

-- | TODO: find a way to change 'drop 15' to a more reliable parsing method
getSurrounding :: String -> Surrounding
getSurrounding str = read ("Surrounding " ++ init (drop 15 (changeSyms str )))::Surrounding

-- | Replaces `"` and `:` so it can be parsed with function "read"
changeSyms :: String -> String
changeSyms str = modifyString ':' '=' (ms '\"' (ms '\\' str))
    where ms ch = modifyString ch space

modifyString :: Char -> Char -> String -> String
modifyString ch r = map (replaceChar ch r)

replaceChar :: Char -> Char-> Char -> Char
replaceChar ch r x | x == ch = r
                   | otherwise = x

space :: Char
space = ' '
