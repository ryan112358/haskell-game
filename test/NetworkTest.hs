{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Conduit
import           Data.Conduit.Network
import           Data.Word8           (toUpper)
import           Data.Conduit.Cereal
import           Data.Serialize
import           Data.ByteString.Char8 (ByteString, pack)

main :: IO ()
main = do
    _ <- server
    client

server = forkTCPServer (serverSettings 4000 "*") $ \appData ->
    appSource appData $$ getInts =$= scanlC (+) 0 =$ printC

getInts :: Conduit ByteString IO Int
getInts = conduitGet get

client = runTCPClient (clientSettings 4000 "localhost") clientAction

clientAction :: AppData -> IO ()
clientAction server = pureSource $$ conduitPut put =$ appSink server

pureSource :: Source IO Int
pureSource = yield 13 >> yield 24 >> yield 11
        

stringify :: (Int, Int) -> String
stringify (a, b) = "First " ++ show a ++ " and second " ++ show b
