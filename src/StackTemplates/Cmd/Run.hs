{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplates.Cmd.Run where

import           RIO
import qualified RIO.ByteString                 as B
import           RIO.Partial                    (fromJust)
import qualified RIO.Text                       as Text

import           Data.Aeson                     (toJSON)
import           Data.Extensible
import qualified Network.Wreq                   as W
import           StackTemplates.Cmd.Options
import           StackTemplates.Data.Hsfiles
import           StackTemplates.Data.Repository
import           StackTemplates.Env
import           StackTemplates.Query
import           System.Environment             (getEnv)

fetchTplList :: Options -> IO ()
fetchTplList = run $ do
  logDebug "run: fetch hsfiles"
  let sOpts = #first @= 100 <: #after @= Nothing <: nil
  repos <- fetchTplListFromGitHub sOpts
  mapM_ (logInfo . display . toStackArg) $ mapHsfilesWithFilter repos

fetchTplListFromGitHub :: SearchOpts -> RIO Env [Repository]
fetchTplListFromGitHub opts = do
  let query = searchQuery "stack-templates in:name" Repository opts
  logDebug $ "query: " <> display query
  result <- (view #search . view #data) <$> postSearchQuery query
  let page  = result ^. #pageInfo
      repos = view #node <$> result ^. #edges
      opts' = opts & #after `set` Just (page ^. #endCursor)
  if | page ^. #hasNextPage -> (repos <>) <$> fetchTplListFromGitHub opts'
     | otherwise            -> pure repos

postSearchQuery :: Text -> RIO Env Response
postSearchQuery query = do
  token <- asks (view #gh_token)
  let pOpts = W.defaults & W.header "Authorization" `set` ["bearer " <> token]
      url = "https://api.github.com/graphql"
  resp <- liftIO $ W.asJSON =<< W.postWith pOpts url (toJSON $ #query @== query <: nil)
  pure $ resp ^. W.responseBody

mapHsfilesWithFilter :: [Repository] -> [Hsfiles]
mapHsfilesWithFilter = mconcat . map (fromRepository GitHub) . filter isStackTemplates

isStackTemplates :: Repository -> Bool
isStackTemplates repo = repo ^. #name == "stack-templates"

fetchRawTpl :: Text -> Options -> IO ()
fetchRawTpl path opts = flip run opts $ do
  let file = readMaybeHsfiles (Text.unpack path)
  logDebug $ display ("run: fetch raw hsfiles " <> path)
  logDebug $ display ("read: " <> tshow file)
  if | isNothing file -> logError $ display ("can't parse input text: " <> path)
     | opts ^. #link  -> logInfo $ display (toUrl $ fromJust file)
     | otherwise      -> B.putStr =<< fetchRawTplBS (toRawUrl $ fromJust file)

fetchRawTplBS :: MonadIO m => Text -> m ByteString
fetchRawTplBS url = do
  resp <- liftIO $ W.get (Text.unpack url)
  let status = resp ^. W.responseStatus . W.statusCode
  if | status == 200 -> pure . toStrictBytes $ resp ^. W.responseBody
     | otherwise     -> pure ""

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
