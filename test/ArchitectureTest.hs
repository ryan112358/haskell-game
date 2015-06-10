{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.Conduit.Network
import Data.Conduit.Cereal
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize

import System.Environment

import Server
import Types

main = do
    args <- getArgs
    case args of
        ["client"] -> runClient
        _ -> runServer

runServer = do
    clients <- newTVarIO Map.empty
    world <- newTVarIO 0
    runTCPServer (serverSettings 4000 "*") (server clients world)

runClient = runTCPClient (clientSettings 4000 "localhost") $ \server -> do
    let updater = forM_ [(1 :: Int)..10] $ \i -> do
        liftIO $ threadDelay 1000000
        yield i
    updateAsync <- async $ updater $$ conduitPut put =$ appSink server
    appSource server $$ asWorld =$ printC
    cancel updateAsync
    putStrLn "done"

asWorld :: Conduit ByteString IO World
asWorld = conduitGet get

