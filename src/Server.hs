{-# LANGUAGE OverloadedStrings #-}

module Server where

import Conduit
import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Conduit.Network
import Data.Conduit.Cereal
import Data.ByteString (ByteString)
import Data.Serialize
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable
import Data.Unique

import Game

runServer = do
    clients <- newTVarIO Map.empty
    world <- newTVarIO 0
    runTCPServer (serverSettings 4000 "*") (server clients world)
    
server :: TVar (Map Unique AppData) -> TVar World -> AppData -> IO ()
server connectionsRef stateRef client = do
    id <- newUnique
    atomically $ modifyTVar' connectionsRef (Map.insert id client)
    handleClient connectionsRef stateRef client
    atomically $ modifyTVar' connectionsRef (Map.delete id)

broadcastWorld :: [AppData] -> World -> IO ()
broadcastWorld clients world = forM_ clients $ \client -> do
    yield world $$ conduitPut put =$ appSink client

asActions :: Conduit ByteString IO Action
asActions = conduitGet get

handleClient :: TVar (Map Unique AppData) -> TVar (World) -> AppData -> IO ()
handleClient connectionsRef stateRef client = 
    appSource client $= asActions $$ awaitForever $ \action -> liftIO $ do
        world <- atomically $ do
            modifyTVar' stateRef (applyActionToWorld action)
            readTVar stateRef
        clientMap <- atomically $ readTVar connectionsRef
        broadcastWorld (Map.elems clientMap) world
        putStrLn $ "Action " ++ show action ++ " received"
