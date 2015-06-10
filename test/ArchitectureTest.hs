{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Control.Concurrent.STM.TVar
import Data.Conduit.Network
import Data.Conduit.Cereal
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize

import System.Environment

import Server

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
    yieldMany [(1 :: Int)..10] $$ conduitPut put =$ appSink server
    putStrLn "done"

