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
  case parseJsonLike (input, 0) of
    Left errorMessage -> Left errorMessage
    Right (parsed, ([], _)) -> Right parsed
    Right (parsed, (_, index)) -> Left $ "Unexpected end of string at index: " ++ show index

-- Determines the value type and passes to an according parser
parseJsonLike :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLike ([], index) = Left $ "Unexpected end of string at index: " ++ show index
parseJsonLike ((x:xs), index)
  | x == '\"' = parseJsonLikeString (x:xs, index)
  | x == '{' = parseJsonLikeObject (x:xs, index)
  | x == '[' = parseArray (x:xs, index)
  | x == 'n' = parseIntegerOrNull (x:xs, index)
  | isDigit x = parseIntegerOrNull (x:xs, index)
  | otherwise = Left $ "No character could be matched at index: " ++ show index

-- Removes the first '\"' in the beggining of the string and passes to parseString
-- If succeeds, returns (JsonlikeString, unparsed String)
parseJsonLikeString :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeString ('\"':xs, index) =
  case parseString ([], (xs, index + 2)) of
    Left errorMessage -> Left errorMessage
    Right (a, (xs, index)) -> Right (JsonLikeString a, (xs, index))
parseJsonLikeString ((_:xs), index) = Left $ "String does not start with \" as expected at index: " ++ show index

-- Universal parser for strings, can be applied both for parsing ObjectKey and for JsonLikeString
-- However, the first '\"' in the beggining of the string has to be removed before passing to this function
-- If succeeds, returns (String, unparsed String)
parseString :: (String, (String, Integer)) -> Either String (String, (String, Integer))
parseString (a, (x:xs, index))
  | x == '\"' = Right (a, (xs, index + 2)) -- Returns if end of string is \"
  | otherwise = parseString ((a ++ [x]), (xs, index + 1)) -- Proceeds to parse chars if not \"
parseString (a, ([], index)) = Left $ "Unfound expected end of string \" at index: " ++ show index

-- Removes the first '{' in the beggining of the object and passes to parseJsonLikeObjectKey with (accumulator, unparsed String)
-- If succeeds, returns (JsonLikeObject, unparsed String)
parseJsonLikeObject :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseJsonLikeObject ('{':xs, index) =
  case parseJsonLikeObjectKey ([], (stripStart (xs, index + 1))) of
    Left errorMessage -> Left errorMessage
    Right (a, (xs, index)) -> Right (JsonLikeObject a, (xs, index))

-- Removes the first '\"' in the beggining of the string and passes to parseString
-- If succeeds, passes to parseJsonLikeObjectValue with (accumulator, unparsed String) and key
parseJsonLikeObjectKey :: ([(String, JsonLike)], (String, Integer)) -> Either String ([(String, JsonLike)], (String, Integer))
parseJsonLikeObjectKey (_, ([], index)) = Left $ "Unexpected end of object at index: " ++ show index
parseJsonLikeObjectKey (a, ('\"':xs, index)) =
  case parseString ([], (xs, index + 2)) of
    Left errorMessage -> Left errorMessage
    Right (key, (xs, index)) -> parseJsonLikeObjectValue (a, (stripStart (xs, index))) key
parseJsonLikeObjectKey (_, (_, index)) = Left $ "Unexpected error at index: " ++ show index

-- Removes the first ':' in the beggining of the string and passes the unparsed String to parseJsonLike
-- If succeeds, passes to continueObjectParse with (accumulator, unparsed String)
parseJsonLikeObjectValue :: ([(String, JsonLike)], (String, Integer)) -> String -> Either String ([(String, JsonLike)], (String, Integer))
parseJsonLikeObjectValue (a, (':':xs, index)) key =
  case parseJsonLike (stripStart (xs, index + 1)) of
    Left errorMessage -> Left errorMessage
    Right (parsed, (xs, index)) -> continueObjectParse ((a ++ [(key, parsed)]), stripStart (xs, index))
parseJsonLikeObjectValue (a, ((_:xs), index)) key = Left $ "Unexpected symbol at index: " ++ show index

-- Determines whether the Object contains more Key:Value pairs (',') or if it has ended ('}')
-- If more, passes to parseJsonLikeObjectKey with (accumulator, unparsed String)
-- If ended, returns (accumulator, unparsed String)
continueObjectParse :: ([(String, JsonLike)], (String, Integer)) -> Either String ([(String, JsonLike)], (String, Integer))
continueObjectParse (a, ([], index)) = Left $ "Unexpected end of object at index: " ++ show index
continueObjectParse (a, ('}':xs, index)) = Right (a, (xs, index + 1))
continueObjectParse (a, (',':xs, index)) = parseJsonLikeObjectKey (a, stripStart (xs, index + 1))
continueObjectParse (a, (_:xs, index)) = Left $ "Unexpected symbol at index: " ++ show index

-- Parses single integer like 123, 111, 0, 1.
-- Throws error if number's length is more than 1 and starts with 0
parseInteger :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseInteger (input, index) =
  let str = takeWhile isDigit input
      strLen = length str
   in if strLen > 1 && head str == '0'
        then Left $ "Number cannot start with 0 at index: " ++ show index
        else Right (JsonLikeInteger (read str), (drop strLen input, index + toInteger strLen))

parseIntegerOrNull (input, index) =
  case take 4 input of
    "null" -> Right (JsonLikeNull, (drop 4 input, index + 4))
    _ -> parseInteger (input, index)

-- Recursively parses any JsonLike elements in array, like "1,2,null,\"string\",{...},[...]]"
-- Throws error if any of the elements' parsing throws error
parseArrayElements :: (String, Integer) -> Either String ([JsonLike], (String, Integer))
parseArrayElements (input, index) =
  let e = parseJsonLike (stripStart (input, index))
   in case e of
        Left errorMessage -> Left errorMessage
        Right (element, (',' : rem, index)) -> case parseArrayElements (stripStart (rem, index + 1)) of
          Left errorMessage -> Left errorMessage
          Right (input, (rem2, index)) -> Right (element : input, (rem2, index))
        Right (element, (rem, index)) -> Right ([element], (rem, index))

-- Parses any array which starts with '[', like "[1,2,null,\"string\",{...someObject..},[...]]"
-- Throws error if array opening bracket '[' or closing bracket ']' is missing
-- Throws error if array elements parsing throws error
parseArray :: (String, Integer) -> Either String (JsonLike, (String, Integer))
parseArray ('[' : x, index) =
  let parsedElements = parseArrayElements (stripStart (x, index))
   in case parsedElements of
        Right (elements, (']' : rem, index)) -> Right (JsonLikeList elements, (rem, index + 1))
        Right (elements, (rem, index)) -> Left $ "Missing array closing bracket ] at index: " ++ show index
        Left errorMessage -> Left errorMessage
parseArray (_, index) = Left $ "Missing array opening bracket [ at index: " ++ show index

-- Drops the beggining of the string while it's a whitespace character
stripStart :: (String, Integer) -> (String, Integer)
stripStart ((x:xs), index)
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
jsonToCoordinates json = case xs of
  Left e -> Left e
  Right xs' -> jsonExtract xs'
  where xs  = jsonGetMapObjects json


-- | Extracts values from values of "bomb" and "surrounding". Parses linked list into [[Int]].
jsonExtract :: [(String, JsonLike)] -> Either String [(String, [[Int]])]
jsonExtract [] = Right []
jsonExtract [([], _)] = Left "Error: object key should not be an empty string."
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
constructList _  = Left "Error: linked list should have \"head\" with a list (or null, if empty) as a value, and \"tail\" with an object as a value."

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
jsonGetMapObjects (JsonLikeObject [("bomb", js), ("surrounding", js')]) = case js' of
  (JsonLikeObject js'') -> Right (("bomb", js) : js'')
  JsonLikeNull -> Right (("bomb", js):[("surrounding", js')])
  _ -> Left "Error: \"surrounding\" doesn't have correct value, it should be null or Json object."
jsonGetMapObjects _ = Left "Error: there should be 2 objects with keys \"bomb\" and \"surrounding\"."
