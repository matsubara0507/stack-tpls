module StackTemplates.Cmd.Run where

import           RIO
import qualified RIO.ByteString                 as B
import           RIO.Directory                  (doesFileExist)
import           RIO.Partial                    (fromJust)
import qualified RIO.Text                       as Text

import           Data.Aeson                     (toJSON)
import           Data.Extensible
import qualified Network.Wreq                   as W
import           StackTemplates.Data.Hsfiles
import           StackTemplates.Data.Repository
import           StackTemplates.Env
import           StackTemplates.Query

fetchTplList :: RIO Env ()
fetchTplList = do
  logDebug "run: fetch hsfiles"
  withUpdate <- asks (view #with_update)
  mapM_ (logInfo . display . toStackArg) =<< getTplList withUpdate

getTplList :: Bool -> RIO Env [Hsfiles]
getTplList updateFlag = do
  cacheFile <- cacheTplsListFile
  isExist   <- doesFileExist cacheFile
  if | isExist && not updateFlag -> readHsfilesList cacheFile
     | otherwise                 -> fetchTplListWithUpdateCache cacheFile

readHsfilesList :: MonadIO m => FilePath -> m [Hsfiles]
readHsfilesList path = do
  ls <- map Text.unpack . Text.lines <$> readFileUtf8 path
  pure $ mapMaybe readMaybeHsfiles ls

fetchTplListWithUpdateCache :: FilePath -> RIO Env [Hsfiles]
fetchTplListWithUpdateCache path = do
  tpls <- mapHsfilesWithFilter <$> fetchTplListFromGitHub sOpts
  writeFileUtf8 path $ Text.unlines (map toStackArg tpls)
  pure tpls
  where
    sOpts = #first @= 100 <: #after @= Nothing <: nil

fetchTplListFromGitHub :: SearchOpts -> RIO Env [Repository]
fetchTplListFromGitHub opts = do
  let query = searchQuery "stack-templates in:name" Repository opts
  logDebug $ "query: " <> display query
  result <- view #search . view #data <$> postSearchQuery query
  let page  = result ^. #pageInfo
      repos = view #node <$> result ^. #edges
      opts' = opts & #after `set` (page ^. #endCursor)
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

fetchRawTpl :: Text -> RIO Env ()
fetchRawTpl path = do
  let file = readMaybeHsfiles (Text.unpack path)
  logDebug $ display ("run: fetch raw hsfiles " <> path)
  logDebug $ display ("read: " <> tshow file)
  onlyLink <- asks (view #only_link)
  if | isNothing file -> logError $ display ("can't parse input text: " <> path)
     | onlyLink       -> logInfo $ display (toUrl $ fromJust file)
     | otherwise      -> B.putStr =<< fetchRawTplBS (toRawUrl $ fromJust file)

fetchRawTplBS :: MonadIO m => Text -> m ByteString
fetchRawTplBS url = do
  resp <- liftIO $ W.get (Text.unpack url)
  let status = resp ^. W.responseStatus . W.statusCode
  if | status == 200 -> pure . toStrictBytes $ resp ^. W.responseBody
     | otherwise     -> pure ""

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
