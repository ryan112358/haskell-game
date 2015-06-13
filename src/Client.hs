{-# LANGUAGE OverloadedStrings #-}

module Client where

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

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game

asWorld :: Conduit ByteString IO World
asWorld = conduitGet get

(width, height) = (400,400) :: (Float, Float)
window = InWindow "Moving Circles" (floor width,floor height) (0,0)

runClient = runTCPClient (clientSettings 4000 "localhost") $ \server -> do
    
    play window white 30 0 render handleEven handleUpdate

