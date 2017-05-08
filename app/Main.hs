{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad                    (forever)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.IORef
import qualified Data.Text                        as T
import           GHC.Generics
import           Lib
import           System.Environment               (getArgs)
import           System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Formatter
import System.IO (getLine, stdout)
import GHC.IO.Handle

type Chain = [Block]
data MySession = EmptySession
data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                       , node            :: LocalNode
                                       , pid             :: ProcessId
                                       } deriving (Generic)

seedport :: String
seedport = "9001"
serviceName :: String
serviceName = "legionservice"

appendSeedPort :: String -> [NodeId]
appendSeedPort thisPort =
  if thisPort == seedport then
    []
  else
    [P2P.makeNodeId $ "localhost:" ++ seedport]

getBlockChain :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Block]
getBlockChain  = do
  (BlockChainState chain _ _) <- getState
  liftIO $ atomicModifyIORef' chain $ \b -> (b, b)

getLatestBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m Block
getLatestBlock = fmap last getBlockChain

addBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => Block -> m ()
addBlock block = do
  (BlockChainState ref _ _) <- getState
  _ <- liftIO $ atomicModifyIORef' ref $ \b -> (b ++ [block], b ++ [block])
  return ()

mineBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => String -> m Block
mineBlock stringData = do
  lastBlock <- getLatestBlock
  mineBlockFrom lastBlock stringData

replaceChain :: MonadIO m => LocalNode -> IORef [Block] -> [Block] -> m ()
replaceChain node chainRef newChain = do
  liftIO $ debugM "legion" "replaced chain called"
  currentChain <- liftIO $ readIORef chainRef
  if (not . isValidChain $ newChain) || (length currentChain >= length newChain)
    then do
      liftIO $ debugM "legion" ("chain is not valid for updating!: " ++ (show newChain))
    else do
      setChain <- liftIO $ atomicModifyIORef' chainRef $ \c -> (newChain, newChain)
      liftIO $ debugM "legion" "chain replaced"
      updatedChain <- liftIO $ readIORef chainRef
      liftIO $ debugM "legion" ("updated chain: " ++ show updatedChain)

runP2P host port = P2P.bootstrapNonBlocking host port (appendSeedPort port) initRemoteTable

initLogger :: IO ()
initLogger = let priority = DEBUG
                 format = \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  in do
    s <- streamHandler stdout priority >>= format
    h <- fileHandler "legion.log" priority >>= format
    updateGlobalLogger rootLoggerName $ (setLevel priority) . (setHandlers [s])

main :: IO ()
main = do
  _ <- initLogger
  debugM "legion" "starting"
  [host, port] <- getArgs
  (node, pid) <- runP2P host port (return ())
  genesis <- initialBlock
  ref <- newIORef [genesis]
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChainState ref node pid)
  _ <- async $ runSpock ((read port :: Int) - 1000) (spock spockCfg Main.app)
  runProcess node $ do
    getSelfPid >>= register serviceName
    forever $ do
      liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
      -- eitherReplaced <- return $ replaceChain broadcastedChain
      -- case eitherReplaced of
      --   Left err -> eturn $ debugM "legion" err
      --   Right newChain -> return $ debugM "legion" "got a new chain and updated ours!"

      (broadcastedChain :: Maybe [Block]) <- expectTimeout 1000000 :: (Process (Maybe [Block]))
      case broadcastedChain of
        Just chain -> do
          liftIO $ debugM "legion" $ "got some stuff: " ++ (show chain)
          replaceChain node ref chain
        Nothing -> do
          liftIO $ debugM "legion" "waiting for p2p input"

app :: SpockM () MySession BlockChainState ()
app = do
  get root $
    text "Hello World!"
  post "block" $ do
    (BlockChainState _ node _) <- getState
    (blockString :: BlockArgs) <- jsonBody'
    liftIO $ debugM "legion" $ show blockString
    block <- mineBlock . blockBody $ blockString
    _ <- addBlock block
    chain <- getBlockChain
    liftIO $ debugM "legion" $ show chain
    liftIO $ runProcess node $ do
      liftIO $ debugM "legion" "about to send some stuff"
      P2P.nsendCapable serviceName chain
      liftIO $ debugM "legion" "sent data"
    text . T.pack . show $ chain
  get "chain" $ do
    chain <- getBlockChain
    text . T.pack . show $ chain
