{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Lib where

import           Control.Monad.Trans
import           Crypto.Hash.SHA256
import           Data.Aeson
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Time.Clock.POSIX
import           GHC.Generics
import Data.Binary

data Block = Block { index        :: Int
                   , previousHash :: String
                   , timestamp    :: Int
                   , blockData    :: String
                   , blockHash    :: String
                   } deriving (Show, Generic)

newtype BlockArgs = BlockArgs{blockBody :: String}
                  deriving (Show, Eq, Generic)

instance ToJSON BlockArgs
instance FromJSON BlockArgs
instance Binary Block

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

isValidNewBlock :: Block -> Block -> Bool
isValidNewBlock prev next
  | index prev + 1 == index next &&
    blockHash prev == previousHash next &&
    blockHash next == calculateBlockHash next = True
  | otherwise = False

isValidChain :: [Block] -> Bool
isValidChain chain = case chain of
  [] -> True
  [_] -> True
  (x:xs) ->
    let next = Prelude.head xs in
      isValidNewBlock x next &&
      isValidChain xs

mineBlockFrom :: (MonadIO m) => Block -> String -> m Block
mineBlockFrom lastBlock stringData = do
  time <- liftIO epoch
  return . addHashToBlock $ Block { index        = index lastBlock + 1
                                  , previousHash = blockHash lastBlock
                                  , timestamp    = time
                                  , blockData    = stringData
                                  , blockHash    = "will be changed"
                                  }
