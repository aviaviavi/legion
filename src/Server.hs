{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Server where

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad                    (forever)
import           Control.Monad.Trans
import           Data.IORef
import           Data.Maybe
import           GHC.Generics
import           Lib
import           System.Log.Logger
import           Text.PrettyPrint.GenericPretty
import           Web.Spock
import           Web.Spock.Config

import qualified Control.Distributed.Backend.P2P  as P2P
import qualified Data.Binary                      as B
import qualified Data.Text                        as T

-- args for running the main application
data MainArgs = MainArgs { httpPort :: String
                         , p2pPort  :: String
                         , seedNode :: Maybe String
                         }

data MySession = EmptySession

-- the state for our application, to be used as a spock state
data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                       , node            :: LocalNode
                                       , pid             :: ProcessId
                                       } deriving (Generic)

-- ADT for data that will be sent across the P2P network
data BlockUpdate = UpdateData Block | ReplaceData [Block] | RequestChain deriving (Generic)
instance B.Binary BlockUpdate

liftDebug :: (MonadIO m) => String -> m ()
liftDebug str = liftIO $ debugM "legion" (show str)

p2pServiceName :: String
p2pServiceName = "updateservice"

-- explicit entry point to run the application, which is useful
-- for our tests
runLegion :: MainArgs -> IO ()
runLegion args = do
  liftDebug "starting"
  (localNode, procId) <- runP2P (p2pPort args) (seedNode args) (return ())
  ref <- maybe (newIORef [initialBlock]) (const $ newIORef []) (seedNode args)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChainState ref localNode procId)
  _ <- async $ runSpock (read (httpPort args) :: Int) (spock spockCfg Server.app)
  -- wait for messages to come in from the p2p network and respond to them
  runProcess localNode $ do
    getSelfPid >>= register p2pServiceName
    liftIO $ threadDelay 1000000
    _ <- if isJust $ seedNode args
    then do
      liftDebug "this is not the initial node, requesting a chain"
      requestChain localNode
    else liftDebug "this is the initial node, not requesting a chain"
    forever $ do
      message <- expect :: Process BlockUpdate
      liftDebug "got a message..."
      case message of
        (ReplaceData chain) -> do
          liftDebug $ "got some stuff to replace: " ++ show chain
          replaceChain ref chain
        (UpdateData block) -> do
          liftDebug $ "got some stuff to add: " ++ show block
          addBlock ref block
        RequestChain -> do
          liftDebug "got chain request"
          sendChain localNode ref

-- retrieve the current block chain
getBlockChain :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Block]
getBlockChain = do
  (BlockChainState chain _ _) <- getState
  liftIO $ readIORef chain

-- retrieve the most recent block in the chain
getLatestBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m Block
getLatestBlock = fmap last getBlockChain

-- add a block to our blockchain, if it's valid
addBlock :: MonadIO m => IORef [Block] -> Block -> m ()
addBlock ref block = do
  chain <- liftIO $ readIORef ref
  if isValidNewBlock (last chain) block
    then do
      liftDebug "adding new block"
      _ <- liftIO $ atomicModifyIORef' ref $ \b -> (b ++ [block], b ++ [block])
      return ()
    else
      liftDebug "new block not valid. skipping"

-- given some data, create a valid block
mineBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => String -> m Block
mineBlock stringData = do
  lastBlock <- getLatestBlock
  mineBlockFrom lastBlock stringData

-- if this chain is valid and longer than what we have, update it.
replaceChain :: MonadIO m => IORef [Block] -> [Block] -> m ()
replaceChain chainRef newChain = do
  currentChain <- liftIO $ readIORef chainRef
  if (not . isValidChain) newChain || length currentChain >= length newChain
    then liftDebug $ "chain is not valid for updating!: " ++ show newChain
    else do
      setChain <- liftIO $ atomicModifyIORef' chainRef $ const (newChain, newChain)
      liftDebug ("updated chain: " ++ show setChain)

-- ask other nodes for their chains
requestChain :: MonadIO m => LocalNode -> m ()
requestChain localNode = liftIO $ runProcess localNode $ do
  liftDebug "requesting chain"
  P2P.nsendPeers p2pServiceName RequestChain

-- sends the entire chain to all nodes in the network.
-- receiving nodes should update if this chain is newer than what they have
sendChain :: MonadIO m => LocalNode -> IORef [Block] -> m ()
sendChain localNode chainRef = liftIO $ runProcess localNode $ do
  liftDebug "emitting chain"
  chain <- liftIO $ readIORef chainRef
  P2P.nsendPeers p2pServiceName $ ReplaceData chain

runP2P port bootstrapNode = P2P.bootstrapNonBlocking "127.0.0.1" port (maybeToList $ P2P.makeNodeId `fmap` bootstrapNode) initRemoteTable

-- spock http endpoint
app :: SpockM () MySession BlockChainState ()
app = do
  get root $
    text "Legion Blockchain Node"
  post "block" $ do
    (BlockChainState ref localNode _) <- getState
    (blockString :: BlockArgs) <- jsonBody'
    liftDebug $ show blockString
    block <- mineBlock . blockBody $ blockString
    _ <- addBlock ref block
    chain <- getBlockChain
    liftDebug $ show chain
    liftIO $ runProcess localNode $ P2P.nsendPeers p2pServiceName $ UpdateData block
    text . T.pack . pretty $ chain
  get "chain" $ do
    chain <- getBlockChain
    text . T.pack . pretty $ chain
