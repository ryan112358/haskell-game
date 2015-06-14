{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (pack)
import System.Environment
import Network

import Server (runServer)
import Client (runClient)

main = withSocketsDo $ do
    args <- getArgs
    case args of
        ["client", host, port] -> do
            putStrLn "Starting client..."
            runClient (pack host) (read port)
        _ -> do
            putStrLn "Starting server..."
            runServer




