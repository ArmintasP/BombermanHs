module Parser2 where

import Data.Char

data JsonLike
  = JsonLikeInteger Integer
  | JsonLikeString String
  | JsonLikeObject [(String, JsonLike)]
  | JsonLikeList [JsonLike]
  | JsonLikeNull
  deriving (Show)

testJsonString :: String
testJsonString = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

-- The starting function for json parsing.
-- Takes: (input) String, where (input) is a json string.
-- Returns: Either String JsonLike,
--   where String - error message, JsonLike - string parsed into JsonLike.
-- HINT: type 'runParser testJsonString' to test the parser
runParser :: String -> Either String JsonLike
runParser [] = Left "Error: Cannot parse empty string"
runParser input = 
  case parseJsonLike (stripStart (input, 0)) of
    Left errorMessage -> Left errorMessage
    Right (parsed, ([], _)) -> Right parsed
    Right (parsed, (_, index)) -> Left $ "Error after index " ++ show index ++ ": Value could not be parsed"

-- Determines the JsonLike value type and passes to an according parser.
-- Takes: (x:xs) String, (index) Integer, where (index) is an index of the last parsed symbol.
-- Returns: Either String (JsonLike, (String, Integer)),
--   where String - error message, JsonLike - string parsed into JsonLike,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseJsonLike :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLike ([], index) = Left $ "Error after index " ++ show index ++ ": Unexpected end of string"
parseJsonLike ((x:xs), index)
  | x == '\"' = parseJsonLikeString (x:xs, index)
  | x == '{' = parseJsonLikeObject (x:xs, index)
  | x == '[' = parseJsonLikeList (x:xs, index)
  | x == 'n' = parseJsonLikeNull (x:xs, index)
  | isDigit x || x == '-' = parseJsonLikeInteger (x:xs, index)
  | otherwise = Left $ "Error after index " ++ show index ++ ": No json value could be matched"

-- Removes the first '\"' in the beggining of the string and passes to parseString.
-- Takes: (x:xs) String, (index) Integer, where (index) is an index of the last parsed symbol.
-- Returns: Either String (JsonLike, (String, Integer)),
--   where String - error message, JsonLike - string parsed into JsonLike,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseJsonLikeString :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeString ('\"':xs, index) =
  case parseString ([], (xs, index + 1)) of
    Left errorMessage -> Left errorMessage
    Right (a, (xs, index)) -> Right (JsonLikeString a, (xs, index))
parseJsonLikeString (_, index) = Left $ "Error after index " ++ show index ++ ": String should start with '\"'"

-- Parser for strings, can be applied both for parsing ObjectKey and for JsonLikeString
-- However, the first '\"' in the beggining of the string has to be removed before passing 
--   to this function.
-- Takes: (a) String, where (a) is an empty list,
--   (x:xs, index) (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index).
-- Returns: Either String (String, (String, Integer)),
--   where String - error message, String - the parsed string,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseString :: (String, (String, Integer)) -> Either String (String, (String, Integer))
parseString (a, (x:xs, index))
  | x == '\"' = Right (a, (xs, index + 1)) -- Returns if end of string is \"
  | otherwise = parseString ((a ++ [x]), (xs, index + 1)) -- Proceeds to parse chars if not \"
parseString (_, (_, index)) = Left $ "Error after index " ++ show index ++ ": Unfound expected end of string '\"'"

-- Removes the first '{' in the beggining of the object and passes to parseJsonLikeObjectKey.
-- Takes: (x:xs, index) (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index).
-- Returns: Either String (JsonLike, (String, Integer)),
--   where String - error message, JsonLike - string parsed into JsonLike,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseJsonLikeObject :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeObject ('{':xs, index) =
  case parseJsonLikeObjectKey ([], (stripStart (xs, index + 1))) of
    Left errorMessage -> Left errorMessage
    Right (a, (xs, index)) -> Right (JsonLikeObject a, (xs, index))
parseJsonLikeObject (_, index) = Left $ "Error after index " ++ show index ++ ": Object should start with '{'"

-- Removes the first '\"' in the beggining of the string and passes to parseString.
-- Takes: (a) [(String, JsonLike)] - where (a) is a JsonLikeObject,
--   (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index).
-- Returns: Either String ([(String, JsonLike)], (String, Integer)),
--   where String - error message, [(String, JsonLike)] - string parsed into JsonLikeObject,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseJsonLikeObjectKey :: ([(String, JsonLike)], (String, Integer)) -> Either String ([(String, JsonLike)], (String, Integer))
parseJsonLikeObjectKey (_, ([], index)) = Left $ "Error after index " ++ show index ++ ": Unexpected end of object"
parseJsonLikeObjectKey (a, ('}':xs, index)) = continueObjectParse (a, ('}':xs, index))
parseJsonLikeObjectKey (a, ('\"':xs, index)) = 
  case parseString ([], (xs, index + 1)) of
    Left errorMessage -> Left errorMessage
    Right (key, (xs, index)) -> parseJsonLikeObjectValue (a, (stripStart (xs, index))) key
parseJsonLikeObjectKey (_, (_, index)) = Left $ "Error after index " ++ show index ++ ": Key should start with '\"'"

-- Removes the first ':' in the beggining of the string and passes the unparsed String to parseJsonLike
-- Takes: (a) [(String, JsonLike)] - where (a) is a JsonLikeObject,
--   (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index),
--   (key) String - where (key) is the key of the JsonLikeObject.
-- Returns: Either String ([(String, JsonLike)], (String, Integer)),
--   where String - error message, [(String, JsonLike)] - string parsed into JsonLikeObject,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseJsonLikeObjectValue :: ([(String, JsonLike)], (String, Integer)) -> String -> Either String ([(String, JsonLike)], (String, Integer))
parseJsonLikeObjectValue (a, (':':xs, index)) key =
  case parseJsonLike (stripStart (xs, index + 1)) of
    Left errorMessage -> Left errorMessage
    Right (parsed, (xs, index)) -> continueObjectParse ((a ++ [(key, parsed)]), stripStart (xs, index))
parseJsonLikeObjectValue (_, (_, index)) key = Left $ "Error after index " ++ show index ++ ": Unfound expected ':' after key in json object"

-- Determines whether the Object contains more Key:Value pairs (',') or if it has ended ('}').
-- Takes: (a) [(String, JsonLike)] - where (a) is a JsonLikeObject,
--   (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index).
-- Returns: Either String ([(String, JsonLike)], (String, Integer)),
--   where String - error message, [(String, JsonLike)] - string parsed into JsonLikeObject,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
continueObjectParse :: ([(String, JsonLike)], (String, Integer)) -> Either String ([(String, JsonLike)], (String, Integer))
continueObjectParse (a, ([], index)) = Left $ "Error after index " ++ show index ++ ": Unexpected end of object"
continueObjectParse (a, ('}':xs, index)) = Right (a, stripStart (xs, index + 1))
continueObjectParse (a, (',':xs, index)) = parseJsonLikeObjectKey (a, stripStart (xs, index + 1))
continueObjectParse (_, (_, index)) = Left $ "Error after index " ++ show index ++ ": Unfound expected '}' or ',' following an object value"

-- Parses single integer like 123, 111, 0, 1.
-- Throws error if number's length is more than 1 and starts with 0
parseJsonLikeInteger :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeInteger (input, index) =
  let str = takeWhile (\x -> isDigit x || x == '-') input
      strLen = length str
   in if strLen == 1 && head str == '-'
        then Left $ "Error after index " ++ show index ++ ": Missing numeric value after -"
      else if strLen > 1 && head str == '0'
        then Left $ "Error after index " ++ show index ++ ": Number cannot start with 0"
      else Right (JsonLikeInteger (read str), (drop strLen input, index + toInteger strLen))

-- Parses null value like null
-- Throws error if 4 first symbols are not equal to null
parseJsonLikeNull :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeNull (input, index) =
  case take 4 input of
    "null" -> Right (JsonLikeNull, (drop 4 input, index + 4))
    _ -> Left $ "Error after index " ++ show index ++ ": Invalid null value"

-- Recursively parses any JsonLike elements in array, like "1,2,null,\"string\",{...},[...]]"
-- Throws error if any of the elements' parsing throws error
parseJsonLikeListValues :: (String, Integer) -> Either String ([JsonLike], (String, Integer))
parseJsonLikeListValues(']' : x, index) =
  Right ([], (']' : x, index + 1))
parseJsonLikeListValues (input, index) =     
  let e = parseJsonLike (stripStart (input, index))
   in case e of
        Left errorMessage -> Left errorMessage
        Right (element, (',' : rem, index)) -> case parseJsonLikeListValues (stripStart (rem, index + 1)) of
          Left errorMessage -> Left errorMessage
          Right ([], (']' : rem, index)) -> Left $ "Error after index " ++ show (index - 1) ++ ": No value found after comma in list"
          Right (input, (rem2, index)) -> Right (element : input, (rem2, index))
        Right (element, (rem, index)) -> Right ([element], (rem, index))

-- Parses any array which starts with '[' and ends with ']', like "[1,2,null,\"string\",{...someObject..},[...]]"
-- Throws error if array opening bracket '[' or closing bracket ']' is missing
-- Throws error if array elements parsing throws error
parseJsonLikeList :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeList ('[' : x, index) =
  let parsedElements = parseJsonLikeListValues (stripStart (x, index))
   in case parsedElements of
        Right (elements, (']' : rem, index)) -> Right (JsonLikeList elements, (rem, index + 1))
        Right (elements, (rem, index)) -> Left $ "Error after index " ++ show index ++ ": Missing array closing bracket ']'"
        Left errorMessage -> Left errorMessage
parseJsonLikeList (_, index) = Left $ "Error after index " ++ show index ++ ": Missing array opening bracket '['"

-- Drops the beggining of the string while it's a whitespace character
-- Takes: (x:xs, index) (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index).
-- Returns: (String, Integer) - the remaining string without whitespaces and the index of the 
--   last parsed symbol.
stripStart :: (String, Integer) -> (String, Integer)
stripStart ([], index) = ([], index)
stripStart ((x:[]), index)
  | isSpace x = stripStart ([], index + 1)
  | otherwise = ([x], index)
stripStart ((x:xs), index)
  | isSpace x = stripStart (xs, index + 1)
  | otherwise = (x:xs, index)

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

finalfun :: [(String, [[JsonLike]])]
finalfun = map f6 pairs
  where pairs = zip (map f4' f2) (map f4 f2)

-- String that is used to test finalfun
parsedJson :: Either String JsonLike
parsedJson = runParser testJsonString

f1 :: Either String JsonLike -> [(String, JsonLike)]
f1 (Right (JsonLikeObject t)) = t
f1 _ = [("", JsonLikeNull)]

f2 :: [(String, JsonLike)]
f2 = f3 $ head (drop 1 (f1 parsedJson))

f3 :: (String, JsonLike) -> [(String, JsonLike)]
f3 (_, JsonLikeObject a) = a 
f3 (_, _) = []

f3' = f4 $ f2 !! 4

f4 :: (String, JsonLike) -> [(String, JsonLike)]
f4 (_ , JsonLikeObject ls) = ls

f4' :: (a, b) -> a
f4' (str, _) = str

f5 :: [(String, JsonLike)] -> [[JsonLike]]
f5 [("head", JsonLikeNull) , ("tail", JsonLikeNull)] = []
f5 [("head", JsonLikeList jval), ("tail", JsonLikeObject obj)] = ([jval] ++ f5 obj)
f5 _ = [[JsonLikeString "buvo klaida"]]

f6 :: (a, [(String, JsonLike)]) -> (a, [[JsonLike]])
f6 (str, xs) = (str, f5 xs)