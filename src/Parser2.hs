module Parser2 where

import Data.Char
import Lib2 (InitData)

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
    Right (parsed, _) -> Left $ ParserError "Error: expected end of string in parseInputString"

parseJsonLike :: String -> Either ParserError (JsonLike, String)
parseJsonLike (x : xs)
  | x == '\"' = parseJsonLikeString $ Right ([], xs)
  | otherwise = undefined
parseJsonLike [] = Left $ ParserError "Error: unexpected end of string in parseJsonLike"

parseJsonLikeString :: Either ParserError (String, String) -> Either ParserError (JsonLike, String)
parseJsonLikeString (Left (ParserError error)) = Left $ ParserError error
parseJsonLikeString (Right (a, (x : xs)))
  | x == '\"' = Right (JsonLikeString a, xs)
  | otherwise = parseJsonLikeString $ Right ((a ++ [x]), xs)

-- Kaip patikrinti, ar išvis egzistuoja end of string \"?
-- "Error: unfound expected end of string \" in parseExpectedChar"

parseArrayElement :: [Char] -> Either ParserError (JsonLike, String)
parseArrayElement e =
  case head e of
    '[' -> parseArray e
    '"' -> parseJsonLike e
    _ -> case take 4 e of
      "null" -> Right (JsonLikeNull, drop 4 e)
      _ -> parseInteger e

parseInteger :: String -> Either ParserError (JsonLike, String)
parseInteger int =
  let str = takeWhile isDigit int
      strLen = length str
   in if strLen > 1 && head str == '0'
        then Left $ ParserError "Error: Illegal number"
        else Right (JsonLikeInteger (read str), drop strLen int)

parseArrayElements :: [Char] -> Either ParserError ([JsonLike], String)
parseArrayElements elements =
  let e = parseArrayElement elements
   in case e of
        Left (ParserError str) -> Left $ ParserError str
        Right (element, ',' : rem) -> case parseArrayElements rem of
          Left str -> Left str
          Right (elements, rem2) -> Right (element : elements, rem2)
        Right (element, rem) -> Right ([element], rem)

parseArray :: [Char] -> Either ParserError (JsonLike, String)
parseArray ('[' : x) =
  let parsedElements = parseArrayElements x
   in case parsedElements of
        Right (elements, ']' : rem) -> Right (JsonLikeList elements, rem)
        Right (elements, rem) -> Left $ ParserError "Error: Missing array closing bracket"
        Left (ParserError str) -> Left $ ParserError str
parseArray _ = Left $ ParserError "Error: Invalid array, it must start with ["
