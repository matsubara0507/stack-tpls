{-# OPTIONS_GHC -fno-warn-orphans #-}

module StackTemplates.Env where

import           RIO
import           RIO.Directory

import           Data.Extensible

type Env = Record
  '[ "logger"      >: LogFunc
   , "gh_token"    >: ByteString
   , "with_update" >: Bool
   , "only_link"   >: Bool
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

cacheTplsListFile :: MonadIO m => m FilePath
cacheTplsListFile = do
  cacheDir <- getXdgDirectory XdgCache "stack-tpls"
  pure $ cacheDir ++ "stack-teplates"
