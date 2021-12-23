{-# LANGUAGE FlexibleInstances #-}

module Parser4 where

import Data.Char
import Data.Either
import Data.List

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, evalState)

data JsonLike
  = JsonLikeInteger Integer
  | JsonLikeString String
  | JsonLikeObject [(String, JsonLike)]
  | JsonLikeList [JsonLike]
  | JsonLikeNull
  deriving (Show)

data ParserError = ParserError
  { index :: Integer,
    message :: String
  }

instance Show ParserError where
  show (ParserError index message) = "Error after index " ++ show index ++ ": " ++ message

type Parser a = ExceptT ParserError (State (String, Integer)) a

testJsonString :: String
testJsonString = "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":[8,1],\"tail\":{\"head\":[6,7],\"tail\":{\"head\":[6,5],\"tail\":{\"head\":[5,8],\"tail\":{\"head\":[5,4],\"tail\":{\"head\":[3,6],\"tail\":{\"head\":[3,4],\"tail\":{\"head\":[2,3],\"tail\":{\"head\":[2,1],\"tail\":{\"head\":[1,8],\"tail\":{\"head\":[1,7],\"tail\":{\"head\":[1,6],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}},\"gates\":{\"head\":null,\"tail\":null},\"ghosts\":{\"head\":null,\"tail\":null},\"wall\":{\"head\":[8,8],\"tail\":{\"head\":[8,6],\"tail\":{\"head\":[8,4],\"tail\":{\"head\":[8,2],\"tail\":{\"head\":[8,0],\"tail\":{\"head\":[7,0],\"tail\":{\"head\":[6,8],\"tail\":{\"head\":[6,6],\"tail\":{\"head\":[6,4],\"tail\":{\"head\":[6,2],\"tail\":{\"head\":[6,0],\"tail\":{\"head\":[5,0],\"tail\":{\"head\":[4,8],\"tail\":{\"head\":[4,6],\"tail\":{\"head\":[4,4],\"tail\":{\"head\":[4,2],\"tail\":{\"head\":[4,0],\"tail\":{\"head\":[3,0],\"tail\":{\"head\":[2,8],\"tail\":{\"head\":[2,6],\"tail\":{\"head\":[2,4],\"tail\":{\"head\":[2,2],\"tail\":{\"head\":[2,0],\"tail\":{\"head\":[1,0],\"tail\":{\"head\":[0,8],\"tail\":{\"head\":[0,7],\"tail\":{\"head\":[0,6],\"tail\":{\"head\":[0,5],\"tail\":{\"head\":[0,4],\"tail\":{\"head\":[0,3],\"tail\":{\"head\":[0,2],\"tail\":{\"head\":[0,1],\"tail\":{\"head\":[0,0],\"tail\":{\"head\":null,\"tail\":null}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}"

runParser :: String -> Either ParserError JsonLike
runParser input = fst $ (runState . runExceptT $ parse) (input, 0)

parse :: Parser JsonLike
parse = do
  (input, _) <- lift get
  when (null input) $ throwE $ ParserError 0 "Cannot parse empty string"
  parsed <- parseJsonLike
  (input', index') <- lift get
  if null input'
    then return parsed
  else throwE $ ParserError index' "Value could not be parsed"

parseJsonLike :: Parser JsonLike
parseJsonLike = do
  (input, index) <- lift get
  when (null input) $ throwE $ ParserError index "Unexpected end of string"
  let x:xs = input
  if x == '\"'
    then parseJsonLikeString
  else if x == '{'
    then parseJsonLikeObject
  else if x == '[' 
    then parseJsonLikeList
  else if x == 'n'
    then parseJsonLikeNull
  else if isDigit x || x == '-'
    then parseJsonLikeInteger
  else throwE $ ParserError index "No json value could be matched"

parseJsonLikeNull :: Parser JsonLike
parseJsonLikeNull = do
  (input, index) <- lift get
  if "null" == (take 4 input)
    then do
      lift . put . stripStart $ (drop 4 input, index + 4)
      return JsonLikeNull
  else throwE $ ParserError index "Invalid null value"

parseJsonLikeInteger :: Parser JsonLike
parseJsonLikeInteger = do
  (input, index) <- lift get
  let str = takeWhile (\x -> isDigit x || x == '-') input
      strLen = length str
      tStr = tail str
  if strLen == 1 && head str == '-'
    then throwE $ ParserError index "Missing numeric value after -"
  else if strLen > 2 && head str == '-' && head tStr == '0'
    then throwE $ ParserError (index + 1) "Number cannot start with 0"
  else if strLen > 1 && head str == '0'
    then throwE $ ParserError index "Number cannot start with 0"
  else do
    lift . put . stripStart $ (drop strLen input, index + toInteger strLen)
    return $ JsonLikeInteger $ read str

parseHex :: Parser String
parseHex = do
  (input, index) <- lift get
  if all isHexDigit (take 4 input)
    then do
      lift . put . stripStart $ (drop 4 input, index + 4)
      return (take 4 input)
  else throwE $ ParserError index "Invalid hex code"

isEscape ch = ch `elem` ['\\', '/', 'b', 'f', 'n', 'r', 't', '\"']

parseEscape :: Parser String
parseEscape = do
  (input, index) <- lift get
  when (null input) $ throwE $ ParserError index "Missing escape character"
  let x:xs = input
  if isEscape x
    then do
      lift . put . stripStart $ (xs, index + 1)
      return [x]
  else if x == 'u'
    then do
      lift . put . stripStart $ (xs, index + 1)
      parsedHex <- parseHex
      return $ [x] ++ parsedHex
  else throwE $ ParserError index "Invalid escape character"

parseString :: Parser String
parseString = do
  let continue parsed = do
        (input, index) <- lift get
        let x:xs = input
        if x == '\"'
          then do
            lift . put . stripStart $ (xs, index + 1)
            return parsed
        else if x == '\\'
          then do
            lift . put . stripStart $ (xs, index + 1)
            parsedEscape <- parseEscape
            continue $ parsed ++ [x] ++ parsedEscape
        else do
          lift . put $ (xs, index + 1)
          continue $ parsed ++ [x]
  result <- continue ""
  return result

parseJsonLikeString :: Parser JsonLike
parseJsonLikeString = do
  (input, index) <- lift get
  let x:xs = input
  if x == '\"'
    then do
      lift . put . stripStart $ (xs, index + 1)
      parsed <- parseString
      return $ JsonLikeString parsed
  else throwE $ ParserError index "String should start with '\"'"

parseJsonLikeObject :: Parser JsonLike
parseJsonLikeObject = do
  (input, index) <- lift get
  when (null input) $ throwE $ ParserError index "Unexpected end of object"
  let x:xs = input
  let continue parsed = do
        keyValue <- parseJsonLikeObjectKeyValue
        (input', index') <- lift get
        if head input' == ','
          then do
            lift . put . stripStart $ (tail input', index' + 1)
            continue $ parsed ++ keyValue
        else if head input' == '}'
          then do
          lift . put . stripStart $ (tail input', index' + 1)
          return $ parsed ++ keyValue
        else throwE $ ParserError index' "Object should end with '}'"   
  if x == '{'
    then do
      lift . put . stripStart $ (xs, index + 1)
      result <- continue []
      return $ JsonLikeObject result
  else throwE $ ParserError index "Object should start with '{'"

parseJsonLikeObjectKeyValue :: Parser [(String, JsonLike)]
parseJsonLikeObjectKeyValue = do
  (input, index) <- lift get
  when (null input) $ throwE $ ParserError index "Unexpected end of object"
  let x:xs = input
  if x == '}'
    then do
      return $ []
  else do
    key <- parseJsonLikeString
    (input', index') <- lift get
    if head input' == ':'
      then do
        lift . put . stripStart $ (tail input', index + 1)
        value <- parseJsonLike
        let (JsonLikeString k) = key
        return $ [(k, value)]
    else throwE $ ParserError index "Unfound expected ':' after key in json object"

parseJsonLikeListValues :: Parser [JsonLike]
parseJsonLikeListValues = do
  (input, index) <- lift get
  when (null input) $ throwE $ ParserError index "Unexpected end of list"
  let x:xs = input
  if (x == ']')
    then do
      lift . put . stripStart $ (xs, index + 1)
      return []
  else do
    result <- parseJsonLike
    (input', index') <- lift get
    when (null input') $ throwE $ ParserError index' "Unexpected end of list"
    if head input' == ','
      then do
        lift . put . stripStart $ ((tail input'), index' + 1)
        result' <- parseJsonLikeListValues
        (input'', index'') <- lift get
        if null result' && head input'' == ']'
          then throwE $ ParserError (index'' - 1) "No value found after comma in list"
        else return $ [result] ++ result'
    else return [result]

parseJsonLikeList :: Parser JsonLike
parseJsonLikeList = do
  (input, index) <- lift get
  let x:xs = input
  if (x == '[')
    then do
      lift . put . stripStart $ (xs, index + 1)
      result <- parseJsonLikeListValues
      (input', index') <- lift get
      if head input' == ']'
        then do
          lift . put . stripStart $ (tail input', index' + 1)
          return $ JsonLikeList result
      else throwE $ ParserError index "Missing array closing bracket ']'"
  else throwE $ ParserError index "Missing array opening bracket '['"

stripStart :: (String, Integer) -> (String, Integer)
stripStart ([], index) = ([], index)
stripStart ([x], index)
  | isSpace x = stripStart ([], index + 1)
  | otherwise = ([x], index)
stripStart (x:xs, index)
  | isSpace x = stripStart (xs, index + 1)
  | otherwise = (x:xs, index)