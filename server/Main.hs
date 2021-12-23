module Main where

import Web.Scotty as S
import Data.UUID
import Data.UUID.V4 (nextRandom)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Class
import Parser3 (JsonLike(..))
import Lib3 (fromJsonLike, Commands(..), Command(..), ToJsonLike (toJsonLike), Direction (Right, Up, Down))
import Data.Either as E
import Data.String.Conversions (cs)
import qualified Data.ByteString.Char8 as B
import System.Random
import GameMap (gameMapCollection, getGameMapWidth, getGameMapHeight, GameMap(..), GameData (mapSize), getGameMapData, gameDataEmpty, applyCommand, explodeBomb, applyFetchCommands)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import GHC.Conc (forkIO)

type Games = M.Map String GameData

main :: IO ()
main = do
  gamesVar <- STM.newTVarIO initializeGames
  scotty 3000 (serverApplication gamesVar)

serverApplication :: STM.TVar Games -> S.ScottyM ()
serverApplication gamesVar = do
  post (capture "/game/play/:uuid") $ do
    addHeader (L.pack "Content-Type") (L.pack "application/json;")
    uuidLazy <- getParameter "uuid"
    rawBody <- body --gameData <- findGame uuid gamesVar
    let uuid = L.unpack uuidLazy
        req = C.unpack rawBody
    response <- liftIO $ playGame req uuid gamesVar

    case response of
      (E.Left e) -> raw $ cs $ show e
      (E.Right js) -> raw $ cs $ extract $ fromJsonLike js

  get (literal "/game/new/random") $ do
    addHeader (L.pack "Content-Type") (L.pack "application/json;")
    (str, gameData, uuid) <- liftIO createNewGameSession
    if uuid == "" then
      raw $ cs ("Couldn't create a game" :: String)
    else do
      liftIO $ insertGame uuid gameData gamesVar
      raw $ cs str

  get (literal "/game/list") $ do
    games <- liftIO $ STM.readTVarIO gamesVar
    let uuids = show $ M.keys games
    addHeader (L.pack "Content-Type") (L.pack "application/json;")
    raw $ cs uuids


insertGame :: String -> GameData -> STM.TVar Games -> IO ()
insertGame uuid gameData gamesVar = STM.atomically $ insertGame' uuid gameData gamesVar


insertGame' :: String -> GameData -> STM.TVar Games -> STM ()
insertGame' uuid gameData gamesVar = do
  games <- STM.readTVar gamesVar
  let newGames = M.insert uuid gameData games
  STM.writeTVar gamesVar newGames

playGame :: String -> String -> STM.TVar Games -> IO (Either String JsonLike)
playGame commandsString uuid gamesVar = do 
  (result, bombStatus) <- STM.atomically $ do
    maybeGame <- findGame uuid gamesVar
    case maybeGame of
      Nothing -> return $ (Left $ "Game with uuid: " ++ uuid ++ " doesn't exist.", False)
      (Just game) -> case parseCommands commandsString of
        (E.Left e) -> return $ (Left e, False)
        (E.Right (cs1, cs2)) -> do
          let (newGame, bombStatus) = applyGameCommands (game, False) cs1  -- First apply commands that plant bomb or move bomberman.
          insertGame' uuid newGame gamesVar

          let shownGame = applyFetchCommands newGame cs2 -- Applying fetching commands and sending game data.
              jsonGameData = toJsonLike shownGame

          return (jsonGameData, bombStatus)
  if bombStatus then do      
    forkIO $ bombThread uuid gamesVar
    return result
  else
    return result


bombThread :: String -> STM.TVar Games -> IO ()
bombThread uuid gamesVar = do
  putStrLn "explos"
  threadDelay 4000000
  STM.atomically $ do
    games  <- STM.readTVar gamesVar
    let newGames = M.adjust explodeBomb uuid games
    STM.writeTVar gamesVar newGames
  putStrLn "Explosion!"


applyGameCommands :: (GameData, Bool) -> [Command] -> (GameData, Bool)
applyGameCommands = foldl (flip applyCommand)

parseCommands :: String -> Either String ([Command], [Command])
parseCommands str = error "Not implemented"

commandsToList :: Commands -> ([Command], [Command]) -> ([Command], [Command])
commandsToList (Commands c Nothing) (xs, xs') = case c of
  FetchSurrounding -> (xs, c:xs')
  FetchBombStatus -> (xs, c:xs')
  FetchBombSurrounding -> (xs, c:xs')
  _ -> (c:xs, xs')
commandsToList (Commands c (Just cs)) ls = commandsToList cs $ commandsToList (Commands c Nothing) ls


findGame :: String -> STM.TVar Games -> STM (Maybe GameData)
findGame uuid gamesVar = do
  games <- STM.readTVar gamesVar
  return $ M.lookup uuid games


initializeGames :: Games
initializeGames = M.empty

getParameter :: String -> ActionM L.Text
getParameter str = param (L.pack str) `rescue` (\str -> return str)

generateUUID :: IO String
generateUUID = do toString <$> nextRandom

createNewGameSession :: IO (String, GameData, String)
createNewGameSession = do
    mapNumber <- getStdRandom (randomR (0, length gameMapCollection - 1)) :: IO Int
    uuid <- liftIO generateUUID
    let gameData = getGameMapData $ gameMapCollection !! mapNumber
        (w, h) = mapSize gameData
        js = fromJsonLike (createGameMapInfoJson uuid w h) :: Either String String
    if isLeft js then
      return (extract js, gameDataEmpty, "")
    else
      return (extract js, gameData, uuid)

extract :: Either String String -> String
extract (E.Left x) = x
extract (E.Right x) = x


createGameMapInfoJson :: String -> Int -> Int -> JsonLike
createGameMapInfoJson uuid w h = JsonLikeObject [
    ("height", JsonLikeInteger $ toInteger h),
    ("uuid", JsonLikeString uuid),
    ("width", JsonLikeInteger $ toInteger w)]
