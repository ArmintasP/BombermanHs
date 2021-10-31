module Parser2 where

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
  | x == '\\' = case parseEscape (a++[x], (xs, index + 1)) of
    Left e -> Left e
    Right (a, (xs, index)) -> parseString (a , (xs, index))
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
parseJsonLikeObject ('{':xs, index) =
  case parseJsonLikeObjectKey ([], stripStart (xs, index + 1)) of
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
    Right (key, (xs, index)) -> parseJsonLikeObjectValue (a, stripStart (xs, index)) key
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
    Right (parsed, (xs, index)) -> continueObjectParse (a ++ [(key, parsed)], stripStart (xs, index))
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
  case take 4 input of
    "null" -> Right (JsonLikeNull, (drop 4 input, index + 4))
    _ -> Left $ "Error after index " ++ show index ++ ": Invalid null value"

-- Recursively parses any JsonLike elements in array, like "1,2,null,\"string\",{...},[...]]"
-- Throws error if any of the elements' parsing throws error
-- Throws error if no JsonLike value is found after , and before [
-- Takes: (String, Integer), 
--    where String - parsable input string, Integer - index of current parsing symbol in starting string
-- Returns: Either String ([JsonLike], (String, Integer))
--    where [JsonLike] - a list of any JsonLike values, (String, Integer) - a tuple of: remainder of unparsed string and index of last parsed symbol
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

-- Parses any array which starts with '[' and ends with ']', like "[1, 2 , null,\"string\",{...},[...]]"
-- Throws error if array opening bracket '[' or closing bracket ']' is missing
-- Throws error if array elements parsing throws error
-- Throws error if jsonLikeListValues parsing throws error
-- Takes: (String, Integer), 
--    where String - parsable input string, Integer - index of current parsing symbol in starting string
-- Returns: Either String (JsonLike, (String, Integer))
--    where JsonLike - a JsonLikeList of any JsonLike values, (String, Integer) - a tuple of: remainder of unparsed string and index of last parsed symbol
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
stripStart ([x], index)
  | isSpace x = stripStart ([], index + 1)
  | otherwise = ([x], index)
stripStart (x:xs, index)
  | isSpace x = stripStart (xs, index + 1)
  | otherwise = (x:xs, index)


--------------------------------------------------------------------------
--  Left String should notify about any errors:
--    if the amount of ints in array (coordinates) is not 2
--    if there are no "bomb" or "surroundings" keys
--    if it cointains wrong types, values, etc.
--  Right Structure should look like the following: [("bomb/bricks/anythingElse", [[x, y]])] [(String, [[Int]])]


-- Pass JsonLike and get a list of coordinates with keys (map element names) on success.
jsonToCoordinates :: JsonLike -> Either String [(String, [[Int]])]
jsonToCoordinates json = xs >>= jsonExtract
  where xs  = jsonGetMapObjects json

-- | Extracts values from values of "bomb" and "surrounding". Parses linked list into [[Int]].
jsonExtract :: [(String, JsonLike)] -> Either String [(String, [[Int]])]
jsonExtract [] = Right []
jsonExtract [([], _)] = Left "Error: object key should not be an empty string."

-- | IMPORTANT: This "bomb" requires a separate case since it's coordinates are given not in a linked list for some reason.
jsonExtract [("bomb", JsonLikeList [JsonLikeInteger x, JsonLikeInteger x'])] =
  if signum x >= 0 && signum x' >= 0 then Right [("bomb", [[fromInteger x, fromInteger x']])]
  else Left "Error: bomb coordinates should be positive"
jsonExtract [("bomb", JsonLikeList xs)] = Left "Error: bomb must have only 2 coordinates: x & y."

jsonExtract [(key, JsonLikeNull)] = Right [(key, [])]
jsonExtract [(key, JsonLikeObject xs)]
  | isLeft coordinates = Left $ head (lefts [coordinates])
  | otherwise = Right [(key, concat coordinates)]
  where coordinates = constructList xs
--jsonExtract [(key, JsonLikeList [])] = Right [(key, [])]
jsonExtract [(key, _)] = Left ("Error: \"" ++ key ++ "\" should have an object (a linked list) as a value, not a list, string or integer.")
jsonExtract (x:xs)
  | hasLeft = Left (intercalate "\n" (lefts [pair, pairs]))
  | otherwise = Right (concat (rights [pair, pairs]))
  where pair = jsonExtract [x]
        pairs = jsonExtract xs
        hasLeft = isLeft pair || isLeft pairs



-- | Constructs a list of coordinates.
constructList :: [(String, JsonLike)] -> Either String [[Int]]
constructList [("head", JsonLikeNull) , ("tail", JsonLikeNull)] = Right []
constructList [("head", JsonLikeList jval), ("tail", JsonLikeObject obj)]
  | hasLeft = Left (intercalate "\n" (lefts [listH, listT]))
  | otherwise = Right (concat (rights [listH, listT]))
  where listH = constructListHead jval
        listT = constructList obj
        hasLeft = isLeft listH || isLeft listT
constructList _  = Left "Error: linked list should have \"head\" with a list (or null, if empty) as a value, and \"tail\" with an object (or null) as a value."

-- | Takes values from a linked list's head.
constructListHead :: [JsonLike] -> Either String [[Int]]
constructListHead [] = Left "Error: if \"head\" has an empty list, its value should be null. Otherwise populate it with coordinates (2 positive integers)."
constructListHead [js, js'] = case (js, js') of
  (JsonLikeInteger x, JsonLikeInteger x') -> if signum x >= 0 && signum x' >= 0 then Right [[fromInteger x, fromInteger x']]
    else Left "Error: \"head\" has a list with negative integer(s)."
  (_, _) -> Left "Error: \"head\" has a list containing other types than positive integers."

constructListHead js = Left "Error: \"head\" must have 2 and only 2 elements that are positive integers that represent coordinates of an object."

-- | Checks if passed json has "surrounding" and "bomb" field. They are crucial for our current program.
jsonGetMapObjects :: JsonLike -> Either String [(String, JsonLike)]
jsonGetMapObjects (JsonLikeObject [("bomb", js), ("surrounding", js')]) = handleBombAndSurroundings ("bomb", js) ("surrounding", js')
jsonGetMapObjects (JsonLikeObject [("surrounding", js'), ("bomb", js)]) = handleBombAndSurroundings ("bomb", js) ("surrounding", js')
jsonGetMapObjects _ = Left "Error: there should be 2 objects with keys \"bomb\" and \"surrounding\"."

handleBombAndSurroundings :: (String, JsonLike) -> (String, JsonLike) -> Either String [(String, JsonLike)]
handleBombAndSurroundings (b, js) (s, js') = case js' of
  (JsonLikeObject js'') -> Right (("bomb", js) : js'')
  JsonLikeNull -> Right [("bomb", js)]
  _ -> Left "Error: \"surrounding\" doesn't have correct value, it should be null or Json object."