{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

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
import           System.Environment               (getArgs)
import           System.IO                        ( stdout)
import           System.Log.Formatter
import           System.Log.Handler               (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Web.Spock
import           Web.Spock.Config

import qualified Control.Distributed.Backend.P2P  as P2P
import qualified Data.Binary                      as B
import qualified Data.Text                        as T

type Chain = [Block]
data MySession = EmptySession
data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                       , node            :: LocalNode
                                       , pid             :: ProcessId
                                       } deriving (Generic)

data MainArgs = MainArgs { httpPort  :: String
                         , p2pPort   :: String
                         , seedNode  :: Maybe String
                         }

data BlockUpdate = UpdateData Block | ReplaceData [Block] | RequestChain deriving (Generic)
instance B.Binary BlockUpdate

liftDebug :: (MonadIO m, Show a) => a -> m ()
liftDebug str = liftIO $ debugM "legion" (show str)
p2pServiceName :: String
p2pServiceName = "updateservice"

getBlockChain :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Block]
getBlockChain  = do
  (BlockChainState chain _ _) <- getState
  liftIO $ atomicModifyIORef' chain $ \b -> (b, b)

getLatestBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m Block
getLatestBlock = fmap last getBlockChain

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

mineBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => String -> m Block
mineBlock stringData = do
  lastBlock <- getLatestBlock
  mineBlockFrom lastBlock stringData

replaceChain :: MonadIO m => LocalNode -> IORef [Block] -> [Block] -> m ()
replaceChain node chainRef newChain = do
  currentChain <- liftIO $ readIORef chainRef
  if (not . isValidChain $ newChain) || (length currentChain >= length newChain)
    then liftDebug ("chain is not valid for updating!: " ++ (show newChain))
    else do
      setChain <- liftIO $ atomicModifyIORef' chainRef $ \c -> (newChain, newChain)
      liftDebug "chain replaced"
      updatedChain <- liftIO $ readIORef chainRef
      liftDebug ("updated chain: " ++ show updatedChain)

requestChain :: MonadIO m => LocalNode -> m ()
requestChain node = liftIO $ runProcess node $ do
  liftDebug "requesting chain"
  P2P.nsendPeers p2pServiceName RequestChain

sendChain :: MonadIO m => LocalNode -> IORef [Block] -> m ()
sendChain node chainRef = liftIO $ runProcess node $ do
  liftDebug "emitting chain"
  chain <- liftIO $ readIORef chainRef
  P2P.nsendPeers p2pServiceName $ ReplaceData chain

runP2P port seedNode = P2P.bootstrapNonBlocking "localhost" port (maybeToList $ P2P.makeNodeId `fmap` seedNode) initRemoteTable

initLogger :: String -> IO ()
initLogger port = let priority = DEBUG
                      format lh = return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  in
    streamHandler stdout priority >>= format >>= \s ->
      fileHandler ("legion" ++ port ++ ".log") priority >>= format >>= \h ->
        updateGlobalLogger rootLoggerName $ (setLevel priority) . (setHandlers [s, h])

main :: IO ()
main = do
  args <- getArgs >>= \a -> case a of
        (h:p:[])   -> return $ MainArgs h p Nothing
        (h:p:i:[]) -> return $ MainArgs h p $ Just i
  -- the argument mostly just a convenient way to have unique log files if we run
  -- several instances locally
  _ <- initLogger $ p2pPort args
  debugM "legion" "starting"
  (node, pid) <- runP2P (p2pPort args) (seedNode args) (return ())
  genesis <- initialBlock
  ref <- maybe (newIORef [genesis]) (const $ newIORef []) (seedNode args)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChainState ref node pid)
  _ <- async $ runSpock ((read (httpPort args) :: Int)) (spock spockCfg Main.app)
  runProcess node $ do
    getSelfPid >>= register p2pServiceName
    liftIO $ threadDelay 1000000
    _ <- if isJust $ seedNode args
    then do
      liftDebug "this is not the initial node, requesting a chain"
      requestChain node
    else liftDebug "this is the initial node, not requesting a chain"
    forever $ do
      message <- (expect :: Process BlockUpdate)
      liftDebug $ "got a message..."
      case message of
        (ReplaceData chain) -> do
          liftDebug $ "got some stuff to replace: " ++ show chain
          replaceChain node ref chain
        (UpdateData block) -> do
          liftDebug $ "got some stuff to add: " ++ show block
          addBlock ref block
        (RequestChain) -> do
          liftDebug "got chain request"
          sendChain node ref

app :: SpockM () MySession BlockChainState ()
app = do
  get root $
    text "Legion Blockchain Node"
  post "block" $ do
    (BlockChainState ref node _) <- getState
    (blockString :: BlockArgs) <- jsonBody'
    liftDebug $ show blockString
    block <- mineBlock . blockBody $ blockString
    _ <- addBlock ref block
    chain <- getBlockChain
    liftDebug chain
    liftIO $ runProcess node $ P2P.nsendPeers p2pServiceName $ UpdateData block
    text . T.pack . show $ chain
  get "chain" $ do
    chain <- getBlockChain
    text . T.pack . show $ chain
