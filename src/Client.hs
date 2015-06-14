{-# LANGUAGE OverloadedStrings #-}

module Client where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString (ByteString)
import Data.Conduit.Network
import Data.Conduit.Cereal
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.Serialize

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Game

asWorld :: Conduit ByteString IO World
asWorld = conduitGet get

window = InWindow "Moving Circles" (gameWidth, gameHeight) (0,0)

runClient host port = runTCPClient (clientSettings port host) $ \server -> do
    worldRef <- newIORef initialWorld
    updater <- async $ appSource server $$ asWorld =$ continuouslyWriteTo worldRef
    playIO 
        window white 30 worldRef
        renderIO
        (handleEventIO $ appSink server)
        ((return.) . flip const)
    cancel updater

renderIO :: IORef World -> IO Picture
renderIO worldRef = do
    world <- readIORef worldRef
    return $ render world

handleEventIO :: Sink ByteString IO () -> Event -> IORef World -> IO (IORef World)
handleEventIO server event worldRef = do
    yield event $= conduitPut put $$ server
    return worldRef

continuouslyWriteTo :: IORef World -> Sink World IO ()
continuouslyWriteTo worldRef = awaitForever $ \world -> liftIO $ do
    putStrLn "Received world update"
    writeIORef worldRef world
