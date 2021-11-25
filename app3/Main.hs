module Main where

import Control.Exception (bracket)
import Control.Lens ((^.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent
import Data.Either as E
import Data.Function ((&))
import Data.List as L (concat, (++))
import Data.String.Conversions (cs)
import Lib3
import Parser2 (JsonLike)
import MapRender (init, render, update, State (State))
import Network.Wreq (post, responseBody)
import Network.Wreq.Lens (Response (..))
import qualified Network.Wreq.Session as Sess
import System.Console.ANSI as ANSI
  ( clearScreen,
    hideCursor,
    setCursorPosition,
    showCursor, clearFromCursorToScreenEnd
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
  _ <- ANSI.clearScreen
  threadDelay 50000
  hSetBuffering stdin NoBuffering
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  bracket
    (ANSI.hideCursor >> Sess.newAPISession)
    (const showCursor)
    ( \sess -> do
        game <- createGame sess :: IO NewGame
        let uuid = gameId game

        gameData <- postCommands uuid sess fetchEverything :: IO CommandsResponse
        playLoop gameData sess uuid (MapRender.init (gameIData game)) launchThread
    )

launchThread :: State -> Sess.Session -> GameId -> IO ()
launchThread state sess uuid = do
  forkIO (bgUpdateMap state sess uuid)
  play state sess uuid

bgUpdateMap :: State -> Sess.Session -> GameId -> IO b
bgUpdateMap state sess uuid = do
  renderGame state
  gameData <- postCommands uuid sess fetchEverything :: IO CommandsResponse
  playLoop gameData sess uuid (MapRender.update state) bgUpdateMap

playLoop :: ToJsonLike a => a -> Sess.Session -> GameId -> (JsonLike -> State) -> (State -> Sess.Session -> GameId -> p) -> p
playLoop gameData sess uuid stateFunction loopfun = case toJsonLike gameData of
   E.Left e -> error e
   E.Right js -> do
     let state = stateFunction js
     loopfun state sess uuid


play :: State -> Sess.Session -> GameId -> IO ()
play state sess uuid = do c <- getChar
                          let action = case c of
                               'w' -> Commands (MoveBomberman Up) (Just fetchEverything)
                               's' -> Commands (MoveBomberman Down) (Just fetchEverything)
                               'a' -> Commands (MoveBomberman Lib3.Left) (Just fetchEverything)
                               'd' -> Commands (MoveBomberman Lib3.Right) (Just fetchEverything)
                               'b' -> Commands PlantBomb (Just fetchEverything)
                               _ -> fetchEverything
                          gameData <- postCommands uuid sess action :: IO CommandsResponse
                          playLoop gameData sess uuid (MapRender.update state) play

renderGame :: MapRender.State -> IO ()
renderGame state = do 
                      threadDelay 50000
                      _ <- ANSI.setCursorPosition 0 0
                      let map = MapRender.render state
                      putStr map

fetchEverything :: Commands
fetchEverything = Commands FetchSurrounding (Just (Commands FetchBombStatus (Just (Commands FetchBombSurrounding Nothing))))
