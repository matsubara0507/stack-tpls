module StackTemplates.Env where

import           RIO
import           RIO.Directory
import           RIO.FilePath

import           Data.Extensible
import           Mix.Plugin.Logger ()


type Env = Record
  '[ "logger"      >: LogFunc
   , "gh_token"    >: ByteString
   , "with_update" >: Bool
   , "only_link"   >: Bool
   ]

cacheTplsListFile :: MonadIO m => m FilePath
cacheTplsListFile = do
  cacheDir <- getXdgDirectory XdgCache "stack-tpls"
  createDirectoryIfMissing True cacheDir
  pure $ cacheDir </> "stack-teplates"
