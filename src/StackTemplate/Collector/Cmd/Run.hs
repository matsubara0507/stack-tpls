{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplate.Collector.Cmd.Run where

import           RIO

import           Data.Aeson                          (toJSON)
import           Data.Extensible
import qualified Network.Wreq                        as W
import           StackTemplate.Collector.Cmd.Options
import           StackTemplate.Collector.Env
import           StackTemplate.Collector.Query
import           System.Environment                  (getEnv)

fetchHsfiles :: Options -> IO ()
fetchHsfiles = run fetchHsfiles'

fetchHsfiles' :: RIO Env ()
fetchHsfiles' = do
  logDebug "run: fetch hsfiles"
  let sOpts = #first @= 100 <: #after @= Nothing <: nil
      query = searchQuery "stack-templates in:name" Repository sOpts
  logDebug $ "query: " <> display query
  resp <- fetchHsfilesFromGitHub query
  logInfo $ displayShow (resp ^. #data ^. #search ^. #repositoryCount)

fetchHsfilesFromGitHub :: Text -> RIO Env Response
fetchHsfilesFromGitHub query = do
  token <- asks (view #gh_token)
  let pOpts = W.defaults & W.header "Authorization" `set` ["bearer " <> token]
      url = "https://api.github.com/graphql"
  resp <- liftIO $ W.asJSON =<< W.postWith pOpts url (toJSON $ #query @== query <: nil)
  pure $ resp ^. W.responseBody

run :: (MonadUnliftIO m, MonadThrow m) => RIO Env () -> Options -> m ()
run f opts = do
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  ghToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  withLogFunc logOpts $ \logger -> do
    let env = #logger   @= logger
           <: #gh_token @= ghToken
           <: nil
    runRIO env f

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
