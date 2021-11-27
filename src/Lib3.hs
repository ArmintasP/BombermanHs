{-# LANGUAGE FlexibleInstances #-}

module Lib3 where

import Data.Either as E (Either (..))
import Data.List as L (lookup)
import Parser3 (JsonLike(..), runParser)
import Data.Data
data InitData = InitData
  { gameWidth :: Int,
    gameHeight :: Int
  }
  deriving (Show)

-- Keep this type as is
type GameId = String

-- Keep these classes as is:
-- You will want to implement instances for them
class ToJsonLike a where
  toJsonLike :: a -> Either String JsonLike

class FromJsonLike a where
  fromJsonLike :: JsonLike -> Either String a

class ContainsGameId a where
  gameId :: a -> GameId

-- Further it is your code: you can change whatever you wish

-- Converts a JsonLike into a String: renders json

parseJsonMessage :: String -> Either String JsonLike
parseJsonMessage = runParser
instance FromJsonLike String where
  fromJsonLike js = E.Right $ renderJson js


renderJson :: JsonLike -> String 
renderJson (JsonLikeInteger num) = show num
renderJson JsonLikeNull = "null"
renderJson (JsonLikeString str) = "\"" ++ str ++ "\""
renderJson (JsonLikeList xs) = "[" ++ renderJsonList xs ++ "]"
renderJson (JsonLikeObject xs) = "{" ++ renderJsonObjects xs ++ "}" 


renderJsonList :: [JsonLike] -> String
renderJsonList [] = ""
renderJsonList [j] = renderJson j
renderJsonList (j:js) = renderJson j ++ "," ++ renderJsonList js       

renderJsonObjects :: [(String, JsonLike)] -> String
renderJsonObjects [] = ""
renderJsonObjects [(str, json)] = "\"" ++ str ++ "\"" ++ ":" ++ renderJson json
renderJsonObjects ((str, json):xs) = renderJsonObjects [(str, json)] ++ "," ++ renderJsonObjects xs



-- Acts as a parser from a String
instance ToJsonLike String where
  toJsonLike = parseJsonMessage

data NewGame = NewGame GameId InitData
  deriving (Show)

class ContainsInitData a where
  gameIData :: a -> InitData
  
instance ContainsGameId NewGame where
  gameId (NewGame gid _) = gid

instance ContainsInitData NewGame where
  gameIData (NewGame _ i) = i

instance FromJsonLike NewGame where
  fromJsonLike o@(JsonLikeObject m) = do uuid <- getwLookup "uuid" getJsonString m
                                         height <-getwLookup "height" getJsonInteger m
                                         width <-getwLookup "width" getJsonInteger m
                                         let idata = InitData width height
                                         E.Right (NewGame uuid idata)
  fromJsonLike v = E.Left $ "Unexpected value: " ++ show v

getwLookup :: Show t => String -> (t -> String -> Either String b) -> [(String, t)] -> Either String b
getwLookup str f js = case L.lookup str js of
  Nothing -> E.Left $ "no " ++ str ++ " field in " ++ show js
  Just js' -> f js' str

getJsonString :: JsonLike -> String -> Either String String
getJsonString (JsonLikeString str) _ = E.Right str
getJsonString _ str = invalidType str

getJsonInteger :: JsonLike -> String -> Either String Int
getJsonInteger (JsonLikeInteger x) _ = E.Right (fromInteger x)
getJsonInteger _ str = invalidType str

invalidType :: String -> Either String b
invalidType = E.Left . (++) "Invalid value type for " 

data Direction = Right | Left | Up | Down
  deriving (Show)

data Command
  = MoveBomberman Direction
  | FetchSurrounding
  | PlantBomb
  | FetchBombStatus
  | FetchBombSurrounding
  deriving (Show)

instance ToJsonLike Command where
  toJsonLike (MoveBomberman dir) = E.Right $ JsonLikeObject [("name", JsonLikeString $ getConst (MoveBomberman dir)), ("direction", JsonLikeString (show dir))]
  toJsonLike c = E.Right $ JsonLikeObject [("name", JsonLikeString $ getConst c)]

getConst :: Show a => a -> String
getConst = head . words . show

data Commands = Commands
  { command :: Command,
    additional :: Maybe Commands
  }
  deriving (Show)

instance ToJsonLike Commands where
  toJsonLike Commands{command = x, additional = Nothing} = do js <-toJsonLike x
                                                              return $ JsonLikeObject [("command", js)]

  toJsonLike Commands{command = x, additional = Just xs} = do x' <- toJsonLike (Commands x Nothing)
                                                              xs' <- toJsonLike xs
                                                              let JsonLikeObject c = x'
                                                              let cs = [("additional", xs')]
                                                              return $ JsonLikeObject (c ++ cs)

-- Note: if using FromJsonLike, always use it with ::String, as there is no need (and logical means) to convert a JsonLike to Commands.
instance FromJsonLike Commands where
  fromJsonLike js = E.Left (show js)

newtype CommandsResponse = CommandsResponse String
  deriving (Show)

instance ToJsonLike CommandsResponse where
  toJsonLike (CommandsResponse str) = toJsonLike str

instance FromJsonLike CommandsResponse where
  fromJsonLike js = E.Right (CommandsResponse (renderJson js))
