{-# LANGUAGE FlexibleInstances #-}

module Lib3 where

import Data.Either as E (Either (..))
import Data.List as L (lookup)
import Lib2 (JsonLike (..), parseJsonMessage)

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
instance FromJsonLike String where
  fromJsonLike _ = E.Right "{}"

-- Acts as a parser from a String
instance ToJsonLike String where
  toJsonLike = Lib2.parseJsonMessage

newtype NewGame = NewGame GameId
  deriving (Show)

instance ContainsGameId NewGame where
  gameId (NewGame gid) = gid

instance FromJsonLike NewGame where
  fromJsonLike o@(JsonLikeObject m) =
    case L.lookup "uuid" m of
      Nothing -> E.Left $ "no uuid field in " ++ show o
      Just (JsonLikeString s) -> E.Right $ NewGame s
      _ -> E.Left "Invalid value type for uuid"
  fromJsonLike v = E.Left $ "Unexpected value: " ++ show v

data Direction = Right | Left | Up | Down
  deriving (Show)

data Command
  = MoveBomberman Direction
  | FetchSurrounding
  | PlantBomb
  | FetchBombStatus
  | FetchBombSurrounding
  deriving (Show)

data Commands = Commands
  { command :: Command,
    additional :: Maybe Commands
  }
  deriving (Show)

instance ToJsonLike Commands where
  toJsonLike _ = E.Right JsonLikeNull

instance FromJsonLike Commands where
  fromJsonLike _ = E.Left "Implement me"

data CommandsResponse = CommandsResponse
  deriving (Show)

instance ToJsonLike CommandsResponse where
  toJsonLike _ = E.Right JsonLikeNull

instance FromJsonLike CommandsResponse where
  fromJsonLike _ = E.Left "Implement me"
