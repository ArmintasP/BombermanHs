module Parser2 where

import Data.Char

data JsonLike
  = JsonLikeInteger Integer
  | JsonLikeString String
  | JsonLikeObject [(String, JsonLike)]
  | JsonLikeList [JsonLike]
  | JsonLikeNull
  deriving (Show)

-- Example json taken from the bomberman-client-2 program
testJsonString :: String
testJsonString = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

-- Test Object parsing without nesting {"key":"value", "key":"value"}
testObjectNotNested :: String
testObjectNotNested = "{\"key1\":\"value1\",\"key2\":\"value2\"}"

-- Test Object parsing with nesting {"key":{"key":"value"}}
testObjectNested :: String
testObjectNested = "{\"key1\":{\"key2\":\"value2\"}}"

-- Test Array parsing
testArrayString :: String
testArrayString = "[\"a\",1,2,3,null,[\"string\",8,null,[5,6]],\"another-string\"]"

-- Returns only the parsed object JsonLike from the tuple (JsonLike, String)
runParser :: String -> Either String JsonLike
runParser [] = Left "Empty string in runParser"
runParser input = 
  case parseJsonLike input of
    Left errorMessage -> Left errorMessage
    Right (parsed, []) -> Right parsed
    Right (parsed, _) -> Left "Unexpected end of string in runParser"

-- Determines the value type and passes to an according parser
parseJsonLike :: String -> Either String (JsonLike, String)
parseJsonLike [] = Left "Unexpected end of string in parseJsonLike"
parseJsonLike (x:xs)
  | x == '\"' = parseJsonLikeString (x:xs)
  | x == '{' = parseJsonLikeObject (x:xs)
  | x == '[' = parseArray (x:xs)
  | x == 'n' = parseIntegerOrNull (x:xs)
  | isDigit x = parseIntegerOrNull (x:xs)
  | otherwise = Left "Unexpected end of string in parseJsonLike"

-- Removes the first '\"' in the beggining of the string and passes to parseString
-- If succeeds, returns (JsonlikeString, unparsed String)
parseJsonLikeString :: String -> Either String (JsonLike, String)
parseJsonLikeString ('\"':xs) = -- Removes \" from the beggining 
  case parseString ([], xs) of
    Left errorMessage -> Left errorMessage
    Right (a, xs) -> Right (JsonLikeString a, xs)
parseJsonLikeString (_:xs) = Left "String does not start with \" as expected in parseJsonLikeString"

-- Universal parser for strings, can be applied both for parsing ObjectKey and for JsonLikeString
-- However, the first '\"' in the beggining of the string has to be removed before passing to this function
-- If succeeds, returns (String, unparsed String)
parseString :: (String, String) -> Either String (String, String)
parseString (a, (x:xs))
  | x == '\"' = Right (a, xs) -- Returns if end of string is \"
  | otherwise = parseString ((a ++ [x]), xs) -- Proceeds to parse chars if not \"
parseString (a, []) = Left "Unfound expected end of string \" in parseString"

-- Removes the first '{' in the beggining of the object and passes to parseJsonLikeObjectKey with (accumulator, unparsed String)
-- If succeeds, returns (JsonLikeObject, unparsed String)
parseJsonLikeObject :: String -> Either String (JsonLike, String)
parseJsonLikeObject ('{':xs) =
  case parseJsonLikeObjectKey ([], xs) of
    Left errorMessage -> Left errorMessage
    Right (a, xs) -> Right (JsonLikeObject a, xs)

-- Removes the first '\"' in the beggining of the string and passes to parseString
-- If succeeds, passes to parseJsonLikeObjectValue with (accumulator, unparsed String) and key
parseJsonLikeObjectKey :: ([(String, JsonLike)], String) -> Either String ([(String, JsonLike)], String)
parseJsonLikeObjectKey (_, []) = Left "Unexpected end of object"
parseJsonLikeObjectKey (a, ('\"':xs)) = 
  case parseString ([], xs) of
    Left errorMessage -> Left errorMessage
    Right (key, xs) -> parseJsonLikeObjectValue (a, xs) key

-- Removes the first ':' in the beggining of the string and passes the unparsed String to parseJsonLike
-- If succeeds, passes to continueObjectParse with (accumulator, unparsed String)
parseJsonLikeObjectValue :: ([(String, JsonLike)], String) -> String -> Either String ([(String, JsonLike)], String)
parseJsonLikeObjectValue (a, (':':xs)) key =
  case parseJsonLike xs of
    Left errorMessage -> Left errorMessage
    Right (parsed, xs) -> continueObjectParse ((a ++ [(key, parsed)]), xs)
parseJsonLikeObjectValue (a, (_:xs)) key = Left "Unexpected symbol in parseJsonLikeObjectValue"

-- Determines whether the Object contains more Key:Value pairs (',') or if it has ended ('}')
-- If more, passes to parseJsonLikeObjectKey with (accumulator, unparsed String)
-- If ended, returns (accumulator, unparsed String)
continueObjectParse :: ([(String, JsonLike)], String) -> Either String ([(String, JsonLike)], String)
continueObjectParse (a, []) = Left "Unexpected end of object in continueParseObject"
continueObjectParse (a, ('}':xs)) = Right (a, xs)
continueObjectParse (a, (',':xs)) = parseJsonLikeObjectKey (a, xs)
continueObjectParse (a, (_:xs)) = Left "Unexpected symbol in continueParseObject"

-- Parses single integer like 123, 111, 0, 1.
-- Throws error if number's length is more than 1 and starts with 0
parseInteger :: String -> Either String (JsonLike, String)
parseInteger int =
  let str = takeWhile isDigit int
      strLen = length str
   in if strLen > 1 && head str == '0'
        then Left "Number cannot start with 0"
        else Right (JsonLikeInteger (read str), drop strLen int)

parseIntegerOrNull string =
  case take 4 string of
    "null" -> Right (JsonLikeNull, drop 4 string)
    _ -> parseInteger string

-- Recursively parses any JsonLike elements in array, like "1,2,null,\"string\",{...},[...]]"
-- Throws error if any of the elements' parsing throws error
parseArrayElements :: [Char] -> Either String ([JsonLike], String)
parseArrayElements elements =
  let e = parseJsonLike elements
   in case e of
        Left errorMessage -> Left errorMessage
        Right (element, ',' : rem) -> case parseArrayElements rem of
          Left errorMessage -> Left errorMessage
          Right (elements, rem2) -> Right (element : elements, rem2)
        Right (element, rem) -> Right ([element], rem)

-- Parses any array which starts with '[', like "[1,2,null,\"string\",{...someObject..},[...]]"
-- Throws error if array opening bracket '[' or closing bracket ']' is missing
-- Throws error if array elements parsing throws error
parseArray :: [Char] -> Either String (JsonLike, String)
parseArray ('[' : x) =
  let parsedElements = parseArrayElements x
   in case parsedElements of
        Right (elements, ']' : rem) -> Right (JsonLikeList elements, rem)
        Right (elements, rem) -> Left "Missing array closing bracket ]"
        Left errorMessage -> Left errorMessage
parseArray _ = Left "Missing array opening bracket ["


--------------------------------------------------------------------------

-- Armintas's flattening functions
-- Run 'finalfun' (no args) to test

-- TODO: finalfun should return: Either String Structure
--  Left String should notify about any errors
--    Return String, if the amount of ints in array (coordinates) is not 2
--  Right Structure should be unwrapped from JsonLike
--    JsonLikeNull should be translated to []
--  Right Structure should look like the following: [("bomb/bricks/anythingElse", [[x, y]])]
-- TODO: refactor into smaller amount of functions
-- TODO: rename functions more accurately and write comments

finalfun = map f6 pairs
  where pairs = zip (map f4' f2) (map f4 f2)

-- String that is used to test finalfun
parsedJson :: Either String JsonLike
parsedJson = runParser testJsonString

f1 :: Either String JsonLike -> [(String, JsonLike)]
f1 (Right (JsonLikeObject t)) = t
f1 _ = [("", JsonLikeNull)]

f2 = f3 $ head (drop 1 (f1 parsedJson))

f3 :: (String, JsonLike) -> [(String, JsonLike)]
f3 (_, JsonLikeObject a) = a 
f3 (_, _) = []

f3' = f4 $ f2 !! 4

f4 (_ , JsonLikeObject ls) = ls

f4' :: (a, b) -> a
f4' (str, _) = str

f5 :: [(String, JsonLike)] -> [[JsonLike]]
f5 [("head", JsonLikeNull) , ("tail", JsonLikeNull)] = []
f5 [("head", JsonLikeList jval), ("tail", JsonLikeObject obj)] = ([jval] ++ f5 obj)
f5 _ = [[JsonLikeString "buvo klaida"]]

f6 (str, xs) = (str, f5 xs)