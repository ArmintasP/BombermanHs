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

-- | Change State the way need but please keep
--  the name of the type, i.e. "State"
data State = State JsonLike InitData
  deriving (Show)

parseJsonMessage :: String -> Either String JsonLike
parseJsonMessage = Prelude.Right . JsonLikeString

-- | Type for defering strings that are actually ParserErrors
data ParserError = ParserError String 
  deriving (Show)

-- | Example json taken from the bomberman-client-2 program
testJsonString :: String
testJsonString = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

-- | Test Object parsing without nesting {"key":"value", "key":"value"}
testObjectNotNested :: String
testObjectNotNested = "{\"key1\":\"value1\",\"key2\":\"value2\"}"

-- | Test Object parsing with nesting {"key":{"key":"value"}}
testObjectNested :: String
testObjectNested = "{\"key1\":{\"key2\":\"value2\"}}"

-- | Returns only the parsed object JsonLike from the tuple (JsonLike, String)
returnParsedValue :: String -> Either ParserError JsonLike
returnParsedValue [] = Left $ ParserError "Error: empty string in parseInputString"
returnParsedValue input = 
  case parseJsonLike input of
    Left (ParserError errorMessage) -> Left (ParserError errorMessage)
    Right (parsed, []) -> Right parsed
    Right (parsed, _) -> Left $ ParserError "Error: unexpected end of string in parseInputString"

-- | Determines the value type and passes to an according parser
parseJsonLike :: String -> Either ParserError (JsonLike, String)
parseJsonLike [] = Left $ ParserError "Error: unexpected end of string in parseJsonLike"
parseJsonLike (x:xs)
  -- | isDigit x = parseJsonLikeInteger (x:xs)
  | x == '\"' = parseJsonLikeString (x:xs)
  | x == '{' = parseJsonLikeObject (x:xs)
  -- | x == '[' = parseJsonLikeList (x:xs)
  -- | x == 'n' = parseJsonLikeNull (x:xs)
  | otherwise = Left $ ParserError "Error: unexpected end of string in parseJsonLike"

-- | Removes the first '\"' in the beggining of the string and passes to parseString
-- | If succeeds, returns (JsonlikeString, unparsed String)
parseJsonLikeString :: String -> Either ParserError (JsonLike, String)
parseJsonLikeString ('\"':xs) = -- Removes \" from the beggining 
  case parseString ([], xs) of
    Left (ParserError error) -> Left (ParserError error)
    Right (a, xs) -> Right (JsonLikeString a, xs)
parseJsonLikeString (_:xs) = Left $ ParserError "Error: string does not start with \" as expected in parseJsonLikeString"

-- | Universal parser for strings, can be applied both for parsing ObjectKey and for JsonLikeString
-- | However, the first '\"' in the beggining of the string has to be removed before passing to this function
-- | If succeeds, returns (String, unparsed String)
parseString :: (String, String) -> Either ParserError (String, String)
parseString (a, (x:xs))
  | x == '\"' = Right (a, xs) -- Returns if end of string is \"
  | otherwise = parseString ((a ++ [x]), xs) -- Proceeds to parse chars if not \"
parseString (a, []) = Left $ ParserError "Error: unfound expected end of string \" in parseString"

-- | Removes the first '{' in the beggining of the object and passes to parseJsonLikeObjectKey with (accumulator, unparsed String)
-- | If succeeds, returns (JsonLikeObject, unparsed String)
parseJsonLikeObject :: String -> Either ParserError (JsonLike, String)
parseJsonLikeObject ('{':xs) =
  case parseJsonLikeObjectKey ([], xs) of
    Left (ParserError errorMessage) -> Left (ParserError errorMessage)
    Right (a, xs) -> Right (JsonLikeObject a, xs)

-- | Removes the first '\"' in the beggining of the string and passes to parseString
-- | If succeeds, passes to parseJsonLikeObjectValue with (accumulator, unparsed String) and key
parseJsonLikeObjectKey :: ([(String, JsonLike)], String) -> Either ParserError ([(String, JsonLike)], String)
parseJsonLikeObjectKey (_, []) = Left $ ParserError "Error: unexpected end of object"
parseJsonLikeObjectKey (a, ('\"':xs)) = 
  case parseString ([], xs) of
    Left (ParserError errorMessage) -> Left (ParserError errorMessage)
    Right (key, xs) -> parseJsonLikeObjectValue (a, xs) key

-- | Removes the first ':' in the beggining of the string and passes the unparsed String to parseJsonLike
-- | If succeeds, passes to continueObjectParse with (accumulator, unparsed String)
parseJsonLikeObjectValue :: ([(String, JsonLike)], String) -> String -> Either ParserError ([(String, JsonLike)], String)
parseJsonLikeObjectValue (a, (':':xs)) key =
  case parseJsonLike xs of
    Left (ParserError errorMessage) -> Left (ParserError errorMessage)
    Right (parsed, xs) -> continueObjectParse ((a ++ [(key, parsed)]), xs)
parseJsonLikeObjectValue (a, (_:xs)) key = Left $ ParserError "Error: Unexpected symbol in parseJsonLikeObjectValue"

-- | Determines whether the Object contains more Key:Value pairs (',') or if it has ended ('}')
-- | If more, passes to parseJsonLikeObjectKey with (accumulator, unparsed String)
-- | If ended, returns (accumulator, unparsed String)
continueObjectParse :: ([(String, JsonLike)], String) -> Either ParserError ([(String, JsonLike)], String)
continueObjectParse (a, []) = Left $ ParserError "Error: Unexpected end of object in continueParseObject"
continueObjectParse (a, ('}':xs)) = Right (a, xs)
continueObjectParse (a, (',':xs)) = parseJsonLikeObjectKey (a, xs)
continueObjectParse (a, (_:xs)) = Left $ ParserError "Error: Unexpected symbol in continueParseObject"