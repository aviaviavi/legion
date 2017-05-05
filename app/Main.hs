{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import           Web.Spock
import Web.Spock
import           Web.Spock.Config

import           Control.Arrow
import           Control.Monad.Trans
import           Crypto.Hash.SHA256
-- import           Data.Aeson
import           Data.IORef
import           Data.Monoid
import           Data.Time.Clock.POSIX
-- import           GHC.Generics
import qualified Data.Text             as T
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Internal as Internal

data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)
newtype BlockChainState = BlockChain (IORef [Block])

data Block = Block { index        :: Int
                   , previousHash :: String
                   , timestamp    :: Int
                   , blockData    :: String
                   , blockHash    :: String
                   } deriving (Show)

-- newtype BlockArgs = BlockArgs{blockBody :: T.Text}
--                   deriving (Show, Eq, Generic)

-- instance ToJSON BlockArgs
-- instance FromJSON BlockArgs

epoch :: IO Int
epoch = round `fmap` getPOSIXTime

hashString :: String -> String
hashString = unpack . hash . pack

calculateBlockHash :: Block -> String
calculateBlockHash (Block i p t b _)  = concatMap hashString [show i, p, show t, b]

addHashToBlock :: Block -> Block
addHashToBlock block = block { blockHash = calculateBlockHash block }

initialBlock :: IO Block
initialBlock = do
  time <- epoch
  let block = Block 0 "0" time "initial data" ""
  return $ block { blockHash = calculateBlockHash block }

getBlockChain :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Block]
getBlockChain  = do
  (BlockChain ref) <- getState
  liftIO $ atomicModifyIORef' ref $ \b -> (b, b)

getLatestBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m Block
getLatestBlock = fmap Prelude.head getBlockChain

isValidNewBlock :: Block -> Block -> Bool
isValidNewBlock prev next
  | index prev + 1 == index next &&
    blockHash prev == previousHash next &&
    blockHash next == calculateBlockHash next = True
  | otherwise = False

addBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => Block -> m ()
addBlock block = do
  (BlockChain ref) <- getState
  _ <- liftIO $ atomicModifyIORef' ref $ \b -> ((block:b), (block:b))
  return ()

mineBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => String -> m Block
mineBlock stringData = do
  time <- liftIO epoch
  lastBlock <- getLatestBlock
  return . addHashToBlock $ Block { index        = index lastBlock + 1
                                  , previousHash = blockHash lastBlock
                                  , timestamp    = time
                                  , blockData    = stringData
                                  , blockHash    = "will be changed"
                                  }

main :: IO ()
main = do
  genesis <- initialBlock
  ref <- newIORef [genesis]
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChain ref)
  runSpock 8080 (spock spockCfg Main.app)

app :: SpockM () MySession BlockChainState ()
app = do
  get root $
    text "Hello World!"
  get ("block" <//> var) $ \(blockString :: String) -> do
    block <- mineBlock blockString
    _ <- addBlock block
    chain <- getBlockChain
    text . T.pack . show $ chain
  get "chain" $ do
    chain <- getBlockChain
    text . T.pack . show $ chain
  -- post "block" $ do
  --   blockBody <- body
  --   addBlock mineBlock
       -- get ("hello" <//> var) $ \name ->
       --     do (BlockChain ref) <- getState
       --        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
       --        text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
