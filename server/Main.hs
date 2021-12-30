{-# LANGUAGE TemplateHaskell #-}

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
import Parser4 (JsonLike(..), runParser)
import Lib3 (fromJsonLike, Commands(..), Command(..), ToJsonLike (toJsonLike), FromJsonLike (fromJsonLike), toCommandList)
import Data.Either as E
import Data.String.Conversions (cs)
import qualified Data.ByteString.Char8 as B
import System.Random
import GameMap (gameMapCollection, getGameMapWidth, getGameMapHeight, GameMap(..), GameData (mapSize),
                getGameMapData, gameDataEmpty, applyCommand, explodeBomb, applyFetchCommands, moveGhosts, Status(..), status)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay, killThread, myThreadId)
import GHC.Conc (forkIO)
import Control.Lens
import Control.Lens.Extras (is)

makePrisms ''Command

type Games = M.Map String (TVar GameData)
port = 3000 -- Server port.

main :: IO ()
main = do
  gamesVar <- STM.newTVarIO initializeGames
  uuidsVar <- STM.newTVarIO initializeUUIDs
  S.scotty port (serverApplication gamesVar uuidsVar)

-- | Listens to HTTP POST/GET
serverApplication :: STM.TVar Games -> STM.TVar [String] -> S.ScottyM ()
serverApplication gamesVar uuidsVar = do
  S.post (capture "/game/play/:uuid") $ do
    addJsonHeader
    uuidLazy <- getParameter "uuid"
    rawBody <- S.body
    let uuid = L.unpack uuidLazy
        req = C.unpack rawBody
    response <- liftIO $ playGame req uuid gamesVar

    case response of
      (E.Left e) -> S.raw $ cs $ show e
      (E.Right js) -> S.raw $ cs $ extract $ fromJsonLike js


  S.get (literal "/game/new/random") $ do
    addJsonHeader
    (str, gameData, uuid) <- liftIO createNewGameSession
    if uuid == "" then
      S.raw $ cs ("Couldn't create a game" :: String)
    else do
      liftIO $ insertGame uuid gameData gamesVar
      liftIO $ insertUUID uuid uuidsVar
      liftIO $ forkIO $ ghostThread uuid gamesVar
      S.raw $ cs str


  S.get (literal "/game/list") $ do
    addJsonHeader
    uuids <- liftIO $ STM.readTVarIO uuidsVar
    S.raw $ cs $ show uuids

addJsonHeader :: ActionM()
addJsonHeader = S.addHeader (L.pack "Content-Type") (L.pack "application/json;")


playGame :: String -> String -> STM.TVar Games -> IO (Either String JsonLike)
playGame commandsString uuid gamesVar = do
  gameM <- STM.atomically $ do
    games  <- STM.readTVar gamesVar
    findGame uuid gamesVar
  
  (result, bombStatus) <- STM.atomically $ playGame' commandsString uuid gameM (\ gameVar game cs1 cs2 -> 
    case GameMap.status game of
      GameWon -> return (toJsonLike game, False)
      GameLost -> return (toJsonLike game, False)
      Playing -> do
        let (newGame, bombStatus) = applyGameCommands (game, False) cs1  -- First apply commands that plant bomb or move bomberman.
        STM.writeTVar gameVar newGame

        let shownGame = applyFetchCommands newGame cs2 -- Applying fetching commands and sending game data.
            jsonGameData = toJsonLike shownGame

        return (jsonGameData, bombStatus))

  if bombStatus then do
    forkIO $ bombThread uuid gamesVar
    return result
  else
    return result

playGame' commandsString uuid gameM func = case gameM of
  Nothing -> return (Left $ "Game with uuid: " ++ uuid ++ " doesn't exist.", False)
  (Just gameVar) -> do
        game <- STM.readTVar gameVar
        case parseCommands commandsString of
          (E.Left e) -> return (Left e, False)
          (E.Right (cs1, cs2)) -> do
            func gameVar game cs1 cs2

ghostThread :: String -> STM.TVar Games -> IO ()
ghostThread uuid gamesVar = launchThread uuid gamesVar 500000 moveGhosts (ghostThread uuid gamesVar)

bombThread :: String -> STM.TVar Games -> IO ()
bombThread uuid gamesVar = launchThread uuid gamesVar 4000000 explodeBomb shutDownThread


launchThread :: String -> STM.TVar Games -> Int -> (GameData -> GameData) -> IO () -> IO ()
launchThread uuid gamesVar delay fun1 fun2 = do
  threadDelay delay

  gameM <- STM.atomically $ do
    games  <- STM.readTVar gamesVar
    findGame uuid gamesVar

  gameStatus <- do
    case gameM of
      Nothing -> return False
      Just gameVar -> STM.atomically $ do
        game <- STM.readTVar gameVar
        case GameMap.status game of
          GameMap.Playing -> do
            let newGame = fun1 game
            STM.writeTVar gameVar newGame
            return True
          _ -> return False

  if gameStatus then
    fun2
  else do
    shutDownThread


insertGame :: String -> GameData -> STM.TVar Games -> IO ()
insertGame uuid gameData gamesVar = STM.atomically $ insertGame' uuid gameData gamesVar

insertGame' :: String -> GameData -> STM.TVar Games -> STM ()
insertGame' uuid gameData gamesVar = do
  games <- STM.readTVar gamesVar
  gameDataVar <- STM.newTVar gameData
  let newGames = M.insert uuid gameDataVar games
  STM.writeTVar gamesVar newGames

insertUUID :: String -> STM.TVar [String] -> IO ()
insertUUID uuid uuidsVar = STM.atomically $ insertUUID' uuid uuidsVar

insertUUID' :: String -> STM.TVar [String] -> STM ()
insertUUID' uuid uuidsVar = do
  uuids <- STM.readTVar uuidsVar
  let newIds = uuid:uuids
  STM.writeTVar uuidsVar newIds


findGame :: String -> STM.TVar Games -> STM (Maybe (TVar GameData))
findGame uuid gamesVar = do
  games <- STM.readTVar gamesVar
  return $ M.lookup uuid games


initializeUUIDs :: [String]
initializeUUIDs =  []

initializeGames :: Games
initializeGames = M.empty

getParameter :: String -> ActionM L.Text
getParameter str = S.param (L.pack str) `S.rescue` (\str -> return str)

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


createGameMapInfoJson :: String -> Int -> Int -> JsonLike
createGameMapInfoJson uuid w h = JsonLikeObject [
    ("height", JsonLikeInteger $ toInteger h),
    ("uuid", JsonLikeString uuid),
    ("width", JsonLikeInteger $ toInteger w)]

applyGameCommands :: (GameData, Bool) -> [Command] -> (GameData, Bool)
applyGameCommands = foldl (flip applyCommand)


-- |
-- | Helper functions
-- |

parseCommands :: String -> Either String ([Command], [Command])
parseCommands str = case runParser str of
  E.Right js -> case fromJsonLike js of
    E.Right (Commands cmds ad) ->
      case countCommand commandList of
        False -> E.Right $ splitCommand commandList
        True -> E.Left "A type of Command occurs more than once"
      where commandList = toCommandList (Commands cmds ad)
    E.Left _ -> E.Left "Jsonlike could not be parsed to Commands"
  E.Left _ -> E.Left "Command string could not be parsed to jsonlike"

isOccurence :: (a -> Bool) -> [a] -> Bool
isOccurence pred list = (<) (length $ filter pred list) 2

countCommand :: [Command] -> Bool
countCommand cmds = False `elem` [mb, fs, pb, fbst, fbs]
  where mb = isOccurence (is _MoveBomberman) cmds
        fs = isOccurence (is _FetchSurrounding) cmds
        pb = isOccurence (is _PlantBomb) cmds
        fbst = isOccurence (is _FetchBombStatus) cmds
        fbs = isOccurence (is _FetchBombSurrounding) cmds

splitCommand :: [Command] -> ([Command], [Command])
splitCommand = span (\x -> (is _MoveBomberman x || is _PlantBomb x))

commandsToList :: Commands -> ([Command], [Command]) -> ([Command], [Command])
commandsToList (Commands c Nothing) (xs, xs') = case c of
  FetchSurrounding -> (xs, c:xs')
  FetchBombStatus -> (xs, c:xs')
  FetchBombSurrounding -> (xs, c:xs')
  _ -> (c:xs, xs')
commandsToList (Commands c (Just cs)) ls = commandsToList cs $ commandsToList (Commands c Nothing) ls

extract :: Either String String -> String
extract (E.Left x) = x
extract (E.Right x) = x

shutDownThread :: IO ()
shutDownThread = do
    id <- myThreadId
    killThread id
    return ()