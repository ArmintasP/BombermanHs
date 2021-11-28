module Main where
import Control.Concurrent.MVar
import Control.Monad
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
import Parser3 (JsonLike)
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
  ANSI.clearScreen
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
  catchInput sess uuid  -- Main thread will be responsible for 'catching user's commands'.

renderingInterval = 100000

bgUpdateMap :: State -> Sess.Session -> GameId -> IO b
bgUpdateMap state sess uuid = do
  renderGame state
  threadDelay renderingInterval
  gameData <- postCommands uuid sess fetchEverything :: IO CommandsResponse
  playLoop gameData sess uuid (MapRender.update state) bgUpdateMap

playLoop :: ToJsonLike a => a -> Sess.Session -> GameId -> (JsonLike -> State) -> (State -> Sess.Session -> GameId -> p) -> p
playLoop gameData sess uuid stateFunction loopfun = case toJsonLike gameData of
   E.Left e -> error e
   E.Right js -> do
     let state = stateFunction js
     loopfun state sess uuid


catchInput :: Sess.Session -> GameId -> IO ()
catchInput sess uuid =  do c <- getChar
                           let action = case c of
                                 'w' -> Commands (MoveBomberman Up) Nothing
                                 's' -> Commands (MoveBomberman Down) Nothing
                                 'a' -> Commands (MoveBomberman Lib3.Left) Nothing
                                 'd' -> Commands (MoveBomberman Lib3.Right) Nothing
                                 'b' -> Commands PlantBomb Nothing
                                 _ -> fetchEverything
                           gameData <- postCommands uuid sess action :: IO CommandsResponse
                           catchInput sess uuid

renderGame :: MapRender.State -> IO ()
renderGame state = do 
  let map = MapRender.render state
  ANSI.setCursorPosition 0 0
  putStrLn map


fetchEverything :: Commands
fetchEverything = Commands FetchSurrounding (Just (Commands FetchBombStatus (Just (Commands FetchBombSurrounding Nothing))))
