{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Lib where

import           Control.Monad.Trans
import           Crypto.Hash                    ( Digest
                                                , SHA256
                                                , digestFromByteString
                                                )
import           Crypto.Hash.SHA256
import           Data.Aeson
import           Data.Binary
import           Data.ByteString.Char8          (pack)
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Text.PrettyPrint.GenericPretty

-- the main data type for our blockchain
data Block = Block { index        :: Int
                   , previousHash :: String
                   , timestamp    :: Int
                   , blockData    :: String
                   , blockHash    :: String
                   } deriving (Show, Eq, Generic)

-- http params to add a block to the chain
newtype BlockArgs = BlockArgs{blockBody :: String}
                  deriving (Show, Eq, Generic)

instance ToJSON BlockArgs
instance FromJSON BlockArgs
instance Binary Block
instance Out Block

-- unix timestamp as an int
epoch :: IO Int
epoch = round `fmap` getPOSIXTime

-- hashes a string and returns a hex digest
sha256 :: String -> Maybe (Digest SHA256)
sha256 = digestFromByteString . hash . pack

-- abstracted hash function that takes a string
-- to hash and returns a hex string
hashString :: String -> String
hashString = maybe (error "Something went wrong generating a hash") show . sha256

calculateBlockHash :: Block -> String
calculateBlockHash (Block i p t b _)  = hashString $ concat [show i, p, show t, b]

-- returns a copy of the block with the hash set
addHashToBlock :: Block -> Block
addHashToBlock block = block { blockHash = calculateBlockHash block }

-- a hardcoded initial block, we need this to make sure all
-- nodes have the same starting point, so we have a hard coded
-- frame of reference to detect validity
initialBlock :: Block
initialBlock = do
  let block = Block 0 "0" 0 "initial data" ""
  block { blockHash = calculateBlockHash block }

-- a new block is valid if its index is 1 higher, its
-- previous hash points to our last block, and its hash is computed
-- correctly
isValidNewBlock :: Block -> Block -> Bool
isValidNewBlock prev next
  | index prev + 1 == index next &&
    blockHash prev == previousHash next &&
    blockHash next == calculateBlockHash next = True
  | otherwise = False

-- a chain is valid if it starts with our hardcoded initial
-- block and every block is valid with respect to the previous
isValidChain :: [Block] -> Bool
isValidChain chain = case chain of
  [] -> True
  [x] -> x == initialBlock
  (x:xs) ->
    let blockPairs = zip chain xs in
      x == initialBlock &&
      all (uncurry isValidNewBlock) blockPairs

-- return the next block given a previous block and some data to put in it
mineBlockFrom :: (MonadIO m) => Block -> String -> m Block
mineBlockFrom lastBlock stringData = do
  time <- liftIO epoch
  return . addHashToBlock $ Block { index        = index lastBlock + 1
                                  , previousHash = blockHash lastBlock
                                  , timestamp    = time
                                  , blockData    = stringData
                                  , blockHash    = "will be changed"
                                  }
