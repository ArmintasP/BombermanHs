module Main where

import Control.Exception (bracket)
import Control.Lens ((^.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Either as E (either)
import Data.Function ((&))
import Data.List as L (concat, (++))
import Data.String.Conversions (cs)
import Lib3
import Network.Wreq (post, responseBody)
import Network.Wreq.Lens (Response (..))
import qualified Network.Wreq.Session as Sess
import System.Console.ANSI as ANSI
  ( clearScreen,
    hideCursor,
    setCursorPosition,
    showCursor,
  )
import System.IO (BufferMode (..), hSetBuffering, hSetEcho, stderr, stdin, stdout)
import Prelude hiding (Left, Right)

-- MANDATORY CODE
host :: String
host = "http://bomberman.homedir.eu"

createGame ::
  (FromJsonLike a) =>
  Sess.Session ->
  IO a
createGame sess = do
  r <- Sess.post sess (host ++ "/v1/game/new/random") B.empty
  let resp = cs $ r ^. responseBody :: String
  return $ toJsonLike resp & e & fromJsonLike & e

postCommands ::
  (FromJsonLike a, ToJsonLike a, FromJsonLike b, ToJsonLike b) =>
  GameId ->
  Sess.Session ->
  a ->
  IO b
postCommands uuid sess commands = do
  let str = toJsonLike commands & e & fromJsonLike & e :: String
  let req = cs str :: B.ByteString
  r <- Sess.post sess (L.concat [host, "/v3/game/", uuid]) req
  let respStr = cs $ r ^. responseBody :: String
  return $ toJsonLike respStr & e & fromJsonLike & e

e :: Either String a -> a
e = E.either error id

-- MANDATORY CODE END

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  bracket
    (ANSI.hideCursor >> Sess.newAPISession)
    (const showCursor)
    ( \sess -> do
        -- you are free to do whatever you want but:
        -- a) reuse sess (connection to the server)
        -- b) use createGame and postCommands to interact with the game server
        game <- createGame sess :: IO NewGame
        let commands = Commands FetchBombSurrounding Nothing :: Commands
        bombSurr <- postCommands (gameId game) sess commands :: IO CommandsResponse
        print bombSurr
    )
