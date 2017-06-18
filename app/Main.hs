{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import           System.Environment               (getArgs)
import           System.IO                        (stdout)
import           System.Log.Formatter
import           System.Log.Handler               (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Server

-- sets up a logger to stdout as well as legion${port}.log
-- the argument to the logger is mostly just a convenient way to have unique log files if we run
-- several instances locally
initLogger :: String -> IO ()
initLogger port = let logPriority = DEBUG
                      format lh = return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  in
    streamHandler stdout logPriority >>= format >>= \s ->
      fileHandler ("legion" ++ port ++ ".log") logPriority >>= format >>= \h ->
        updateGlobalLogger rootLoggerName $ setLevel logPriority . setHandlers [s, h]

main :: IO ()
main = do
  args <- getArgs >>= \a -> case a of
        [h,p] -> return $ MainArgs h p Nothing
        [h,p,i] -> return $ MainArgs h p $ Just i
        _ -> fail "Usage:\n\n$ legion-exe httpPort p2pPort [optional bootstrap p2p address]\n\n\n"
  _ <- initLogger $ p2pPort args
  runLegion args


