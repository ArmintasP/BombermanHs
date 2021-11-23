module Parser3 where

import Data.Char
import Data.Either
import Data.List

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
runParser input = do
  (parsed, (input, index)) <- parseJsonLike (stripStart (input, 0))
  if input == []
    then return parsed
    else Left $ "Error after index " ++ show index ++ ": Value could not be parsed"

-- Determines the JsonLike value type and passes to an according parser.
-- Takes: (x:xs) String, (index) Integer, where (index) is an index of the last parsed symbol.
-- Returns: Either String (JsonLike, (String, Integer)),
--   where String - error message, JsonLike - string parsed into JsonLike,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseJsonLike :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLike ([], index) = Left $ "Error after index " ++ show index ++ ": Unexpected end of string"
parseJsonLike (x:xs, index)
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
parseJsonLikeString ('\"':xs, index) = do
  (a, (xs, index)) <- parseString ([], (xs, index + 1))
  return (JsonLikeString a, (xs, index))
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
  | x == '\\' = do
    parsedEscape <- parseEscape (a ++ [x], (xs, index + 1))
    result <- parseString parsedEscape
    return result
  | otherwise = parseString (a ++ [x], (xs, index + 1)) -- Proceeds to parse chars if not \"
parseString (_, (_, index)) = Left $ "Error after index " ++ show index ++ ": Unfound expected end of string '\"'"

parseEscape :: (String, (String, Integer)) -> Either String (String, (String, Integer))
parseEscape (a, (x:xs, index))
  | isEscape x = Right (a ++ [x], (xs, index + 1))
  | x == 'u' = parseHex (a++[x], (xs, index + 1))
  | otherwise = Left $ "Error after index " ++ show index ++ ": Invalid escape character at index"
parseEscape (a, ([], index)) = Left $ "Error after index " ++ show index ++ ": Missing escape character at index"

parseHex :: (String, (String, Integer)) -> Either String (String, (String, Integer))
parseHex (a, (c1:c2:c3:c4:xs, index))
  | all isHexDigit [c1, c2, c3, c4] = Right (a ++ [c1, c2, c3, c4], (xs, index + 4))
  | otherwise = Left $ "Error after index " ++ show index ++ ": Invalid hex code at index"
parseHex (_, (_, index)) = Left $ "Error after index "++ show index ++ ": Missing hex code characters at index"

isEscape ch = ch `elem` ['\\', '/', 'b', 'f', 'n', 'r', 't', '\"']

-- Removes the first '{' in the beggining of the object and passes to parseJsonLikeObjectKey.
-- Takes: (x:xs, index) (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index).
-- Returns: Either String (JsonLike, (String, Integer)),
--   where String - error message, JsonLike - string parsed into JsonLike,
--   (String, Integer) - remainder of the unparsed string and the index of the last parsed symbol.
parseJsonLikeObject :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeObject ('{':xs, index) = do
  (a, (xs, index)) <- parseJsonLikeObjectKey ([], stripStart (xs, index + 1))
  return (JsonLikeObject a, (xs, index))
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
parseJsonLikeObjectKey (a, ('\"':xs, index)) = do
  (key, (xs, index)) <- parseString ([], (xs, index + 1))
  result <- parseJsonLikeObjectValue (a, stripStart (xs, index)) key
  return result
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
parseJsonLikeObjectValue (a, (':':xs, index)) key = do
  (parsed, (xs, index)) <- parseJsonLike (stripStart (xs, index + 1))
  result <- continueObjectParse (a ++ [(key, parsed)], stripStart (xs, index))
  return result
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

-- Parses single integer like 123, 111, 0, 1, -19.
-- Throws error if number's length is more than 1 and starts with 0 or has - with no numeric value
-- Takes: (String, Integer), 
--    where String - parsable input string, Integer - index of current parsing symbol in starting string
-- Returns: Either String (JsonLike, (String, Integer))
--    where JsonLike - any JsonLikeInteger value, (String, Integer) - a tuple of: remainder of unparsed string and index of last parsed symbol
parseJsonLikeInteger :: ([Char], Integer) -> Either [Char] (JsonLike, ([Char], Integer))
parseJsonLikeInteger (input, index) =
  let str = takeWhile (\x -> isDigit x || x == '-') input
      strLen = length str
   in if strLen == 1 && head str == '-'
        then Left $ "Error after index " ++ show index ++ ": Missing numeric value after -"
      else if strLen > 1 && head str == '0'
        then Left $ "Error after index " ++ show index ++ ": Number cannot start with 0"
      else Right (JsonLikeInteger (read str), (drop strLen input, index + toInteger strLen))

-- Parses null value like null.
-- Throws error if 4 first symbols are not equal to null
-- Takes: (String, Integer), 
--    where String - parsable input string, Integer - index of current parsing symbol in starting string
-- Returns: Either String (JsonLike, (String, Integer))
--    where JsonLike - JsonLikeNull value, (String, Integer) - a tuple of: remainder of unparsed string and index of last parsed symbol
parseJsonLikeNull :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeNull (input, index) =
  if "null" == (take 4 input)
    then return (JsonLikeNull, (drop 4 input, index + 4))
    else Left $ "Error after index " ++ show index ++ ": Invalid null value"

-- Recursively parses any JsonLike elements in array, like "1,2,null,\"string\",{...},[...]]"
-- Throws error if any of the elements' parsing throws error
-- Throws error if no JsonLike value is found after , and before [
-- Takes: (String, Integer), 
--    where String - parsable input string, Integer - index of current parsing symbol in starting string
-- Returns: Either String ([JsonLike], (String, Integer))
--    where [JsonLike] - a list of any JsonLike values, (String, Integer) - a tuple of: remainder of unparsed string and index of last parsed symbol
parseJsonLikeListValues :: (String, Integer) -> Either String ([JsonLike], (String, Integer))
parseJsonLikeListValues([], index) = Left $ "Error after index " ++ show index ++ ": Unexpected end of list"
parseJsonLikeListValues(']':xs, index) = Right ([], (']':xs, index + 1))
parseJsonLikeListValues (input, index) = do
  (parsed, (xs, index)) <- parseJsonLike (stripStart (input, index))
  if head xs == ','
    then do 
      (parsed, (xs, index)) <- parseJsonLikeListValues (stripStart (tail xs, index + 1))
      if null parsed && head xs == ']'
        then Left $ "Error after index " ++ show index ++ ": No value found after comma in list"
        else return (parsed, (xs, index))
    else return ([parsed], (xs, index))

-- Parses any array which starts with '[' and ends with ']', like "[1, 2 , null,\"string\",{...},[...]]"
-- Throws error if array opening bracket '[' or closing bracket ']' is missing
-- Throws error if array elements parsing throws error
-- Throws error if jsonLikeListValues parsing throws error
-- Takes: (String, Integer), 
--    where String - parsable input string, Integer - index of current parsing symbol in starting string
-- Returns: Either String (JsonLike, (String, Integer))
--    where JsonLike - a JsonLikeList of any JsonLike values, (String, Integer) - a tuple of: remainder of unparsed string and index of last parsed symbol
parseJsonLikeList :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeList ('[':xs, index) = do
  (parsed, (xs, index)) <- parseJsonLikeListValues (stripStart (xs, index))
  if head xs == ']'
    then return (JsonLikeList parsed, (tail xs, index + 1))
    else Left $ "Error after index " ++ show index ++ ": Missing array closing bracket ']'"
parseJsonLikeList (_, index) = Left $ "Error after index " ++ show index ++ ": Missing array opening bracket '['"

-- Drops the beggining of the string while it's a whitespace character
-- Takes: (x:xs, index) (String, Integer) - the unparsed string (x:xs) and the index of the last 
--   parsed symbol (index).
-- Returns: (String, Integer) - the remaining string without whitespaces and the index of the 
--   last parsed symbol.
stripStart :: (String, Integer) -> (String, Integer)
stripStart ([], index) = ([], index)
stripStart ([x], index)
  | isSpace x = stripStart ([], index + 1)
  | otherwise = ([x], index)
stripStart (x:xs, index)
  | isSpace x = stripStart (xs, index + 1)
  | otherwise = (x:xs, index)
