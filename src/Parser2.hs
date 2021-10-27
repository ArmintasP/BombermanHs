module Parser2 where

import Lib2 (InitData)
import Data.Char

data JsonLike
  = JsonLikeInteger Integer
  | JsonLikeString String
  | JsonLikeObject [(String, JsonLike)]
  | JsonLikeList [JsonLike]
  | JsonLikeNull
  deriving (Show)

data ParserError = ParserError String 
  deriving (Show)

-- | Change State the way need but please keep
--  the name of the type, i.e. "State"
data State = State JsonLike InitData
  deriving (Show)

parseJsonMessage :: String -> Either String JsonLike
parseJsonMessage = Prelude.Right . JsonLikeString

testJsonString :: String
testJsonString = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

parseInputString :: String -> Either ParserError JsonLike
parseInputString [] = Left $ ParserError "Error: empty string in parseInputString"
parseInputString input = 
  case parseJsonLike input of
    Left (ParserError errorMessage) -> Left (ParserError errorMessage)
    Right (parsed, []) -> Right parsed
    Right (parsed, _) -> Left $ ParserError "Error: unexpected end of string in parseInputString"

parseJsonLike :: String -> Either ParserError (JsonLike, String)
parseJsonLike [] = Left $ ParserError "Error: unexpected end of string in parseJsonLike"
parseJsonLike (x:xs)
  -- | x == '{' = parseJsonLikeObject $ Right ([], xs)
  | x == '\"' = parseJsonLikeString $ Right ([], x:xs)
  | otherwise = Left $ ParserError "Error: unexpected end of string in parseJsonLike"

-- Argument left as Either in case we would want to chain several functions
parseJsonLikeString :: Either ParserError (String, String) -> Either ParserError (JsonLike, String)
parseJsonLikeString (Right (a, ('\"':xs))) =
    case parseString (Right (a, xs)) of
      Left (ParserError error) -> Left (ParserError error)
      Right (a, xs) -> Right (JsonLikeString a, xs)
parseJsonLikeString (Right (a, (_:xs))) = Left $ ParserError "Error: string does not start with \" as expected in parseJsonLikeString"

parseString :: Either ParserError (String, String) -> Either ParserError (String, String)
parseString (Right (a, (x:xs)))
  | x == '\"' = Right (a, xs) -- Returns if end of string \"
  | otherwise = parseString $ Right ((a ++ [x]), xs) -- Proceeds to parse chars if not \"
parseString (Right (a, [])) = Left $ ParserError "Error: unfound expected end of string \" in parseString"

parseJsonLikeObject :: Either ParserError (String, String) -> Either ParserError (JsonLike, String)
parseJsonLikeObject (Right (a, ('{':xs))) = undefined
parseJsonLikeObject (Right (a, ('}':xs))) = undefined -- empty object
parseJsonLikeObject (Right (a, xs)) = 
  case parseJsonLikeObjectKey (Right (a, xs)) of
    Left (ParserError errorMessage) -> Left (ParserError errorMessage)
    Right (a, xs) -> Right (JsonLikeString a, xs)

parseJsonLikeObjectKey :: Either ParserError (String, String) -> Either ParserError (String, String)
parseJsonLikeObjectKey (Right (a, xs)) = parseString (Right (a, xs))