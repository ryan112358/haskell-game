{-# LANGUAGE OverloadedStrings #-}

module Client where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.Conduit.Network
import Data.Conduit.Cereal
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Game

asWorld :: Conduit ByteString IO World
asWorld = conduitGet get

window = InWindow "Moving Circles" (gameWidth, gameHeight) (0,0)

runClient host port= runTCPClient (clientSettings port host) $ \server -> do
    worldRef <- newTVarIO undefined
    updater <- async $ appSource server $$ asWorld =$ continuouslyWriteTo worldRef
    playIO 
        window white 30 worldRef
        renderIO
        (handleEventIO $ appSink server)
        ((return.) . flip const)
    cancel updater

renderIO :: TVar World -> IO Picture
renderIO worldRef = do
    world <- atomically $ readTVar worldRef
    return $ render world

handleEventIO :: Sink ByteString IO () -> Event -> TVar World -> IO (TVar World)
handleEventIO server event worldRef = do
    yield event $= conduitPut put $$ server
    return worldRef

continuouslyWriteTo :: TVar World -> Sink World IO ()
continuouslyWriteTo worldRef = awaitForever $ \world -> liftIO $ do
    atomically $ writeTVar worldRef world
