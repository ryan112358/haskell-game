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

import Types

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
            modifyTVar' stateRef (+action)
            readTVar stateRef
        clientMap <- atomically $ readTVar connectionsRef
        broadcastWorld (Map.elems clientMap) world
        putStrLn $ "Action " ++ show action ++ " received"
