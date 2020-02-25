module Main where

import           Paths_stack_tpls     (version)
import           RIO
import           RIO.Directory

import           Configuration.Dotenv (Config (..), defaultConfig, loadFile)
import           Data.Extensible
import           GetOpt               (withGetOpt')
import           Mix
import           Mix.Plugin.Logger    as MixLogger
import           StackTemplates.Cmd
import           System.Environment   (getEnv)
import           Version

main :: IO ()
main = withGetOpt' "[options] [show filename]" opts $ \r args usage -> do
  homeDir <- getHomeDirectory
  dotenvPaths <- filterM doesFileExist [".env", homeDir ++ "/.env"]
  _ <- loadFile $ defaultConfig { configPath = dotenvPaths }
  case toCmd (#input @= args <: r) of
    Just PrintVersion             -> hPutBuilder stdout (Version.build version)
    Just PrintHelp                -> hPutBuilder stdout (fromString usage)
    Just (FetchRawTpl path opts') -> Mix.run (toPlugin opts') (fetchRawTpl path)
    Just (FetchTplList opts')     -> Mix.run (toPlugin opts') fetchTplList
    Nothing                       -> mistake args
  where
    opts = #version @= versionOpt
        <: #help    @= helpOpt
        <: #verbose @= verboseOpt
        <: #list    @= listOpt
        <: #link    @= linkOpt
        <: #update  @= updateOpt
        <: nil
    mistake args = do
      hPutBuilder stdout
        (fromString $ "undefined subcommand: " <> show args <> "\n")
      exitFailure

toPlugin :: MonadUnliftIO m => Options -> Mix.Plugin () m Env
toPlugin opts = hsequence
   $ #logger      <@=> MixLogger.buildPlugin logOpts
  <: #gh_token    <@=> liftIO (fromString <$> getEnv "GH_TOKEN")
  <: #with_update <@=> pure (opts ^. #update)
  <: #only_link   <@=> pure (opts ^. #link)
  <: nil
  where
    logOpts = #handle @= stdout <: #verbose @= opts ^. #verbose <: nil
