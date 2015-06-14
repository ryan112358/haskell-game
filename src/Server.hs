{-# LANGUAGE OverloadedStrings #-}

module Server where

import Conduit
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Conduit.Network
import Data.Conduit.Cereal
import Data.ByteString (ByteString)
import Data.Serialize
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Traversable
import Data.Unique

import Network.Socket

import Graphics.Gloss.Interface.Pure.Game

import Game

runServer = do
    playerNums <- newTVarIO Map.empty
    clients <- newTVarIO Map.empty
    world <- newTVarIO initialWorld
    simulation <- async $ simulateWorld world 30
    runTCPServer (serverSettings 4000 "*") (server playerNums clients world)
    cancel simulation
    
server :: TVar (Map SockAddr Int) -> TVar (Map Unique AppData) -> TVar World -> AppData -> IO ()
server playerNumMapRef connectionsRef worldRef client = do
    let addr = appSockAddr client
    id <- newUnique
    playerNum <- atomically $ do
        modifyTVar' connectionsRef (Map.insert id client)
        nums <- readTVar playerNumMapRef
        let playerNum = Map.size nums + 1
        let nums' = Map.insertWith (flip const) addr playerNum nums
        writeTVar playerNumMapRef nums'
        return $ nums' ! addr
    putStrLn $ "Player " ++ show playerNum ++ " connected."
    handleClient connectionsRef worldRef playerNum client

broadcastWorld :: [AppData] -> World -> IO ()
broadcastWorld clients world = forM_ clients $ \client -> do
    yield world $$ conduitPut put =$ appSink client

asEvents :: Conduit ByteString IO Event
asEvents = conduitGet get

handleClient :: TVar (Map Unique AppData) -> TVar (World) -> Int -> AppData -> IO ()
handleClient connectionsRef worldRef playerNum client = 
    appSource client $= asEvents $$ awaitForever $ \event -> liftIO $ do
        world <- atomically $ do
            modifyTVar' worldRef (handleEvent playerNum event)
            readTVar worldRef
        clientMap <- atomically $ readTVar connectionsRef
        broadcastWorld (Map.elems clientMap) world
        putStrLn $ "Action " ++ show (playerNum, event) ++ " received"

simulateWorld :: TVar World -> Int -> IO ()
simulateWorld worldRef fps = let
    delay = 1000000 `div` fps 
    in forever $ do
        threadDelay delay
        atomically $ modifyTVar' worldRef (update $ fromIntegral delay / 1000000)
