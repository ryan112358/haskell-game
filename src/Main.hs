
module Main where

import System.Environment
import Network

import Server (runServer)
import Client (runClient)

main = withSocketsDo $ do
    args <- getArgs
    case args of
        ["client"] -> runClient
        _ -> runServer




