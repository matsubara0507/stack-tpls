module StackTemplates.Cmd.Run where

import           RIO
import qualified RIO.ByteString                as B
import           RIO.Directory                 (doesFileExist)
import qualified RIO.Text                      as Text

import           Network.HTTP.Req              as Req
import           StackTemplates.Data.Hsfiles
import           StackTemplates.Env
import qualified StackTemplates.GitHub.GraphQL as GitHub

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
  tpls <- mapHsfilesWithFilter <$> fetchTplListFromGitHub
  writeFileUtf8 path $ Text.unlines (map toStackArg tpls)
  pure tpls

fetchTplListFromGitHub :: RIO Env [GitHub.Repository]
fetchTplListFromGitHub = do
  token <- asks (view #gh_token)
  result <- GitHub.searchRepository token "stack-templates in:name"
  case result of
    Left err -> logError (fromString err) >> pure []
    Right r  -> pure r

mapHsfilesWithFilter :: [GitHub.Repository] -> [Hsfiles]
mapHsfilesWithFilter = mconcat . map (fromRepository GitHub) . filter isStackTemplates

isStackTemplates :: GitHub.Repository -> Bool
isStackTemplates repo = repo ^. #name == "stack-templates"

fetchRawTpl :: Text -> RIO Env ()
fetchRawTpl path = do
  logDebug $ display ("run: fetch raw hsfiles " <> path)
  onlyLink <- asks (view #only_link)
  case readMaybeHsfiles (Text.unpack path) of
    Nothing              -> logError $ display ("can't parse input text: " <> path)
    Just file | onlyLink -> logInfo $ display (toUrl file)
    Just file            -> B.putStr =<< fetchRawTplBS file

fetchRawTplBS :: MonadIO m => Hsfiles -> m ByteString
fetchRawTplBS file = runReq defaultHttpConfig $ do
  responseBody <$> req GET (toRawUrl' file) NoReqBody bsResponse mempty

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
