module Main where

import           Paths_stack_tpls     (version)
import           RIO
import           RIO.Directory

import           Configuration.Dotenv (Config (..), defaultConfig, loadFile)
import           Data.Extensible
import           GetOpt               (withGetOpt')
import           StackTemplates.Cmd
import           Version

main :: IO ()
main = withGetOpt' "[options] [show filename]" opts $ \r args usage -> do
  homeDir <- getHomeDirectory
  dotenvPaths <- filterM doesFileExist [".env", homeDir ++ "/.env"]
  _ <- loadFile $ defaultConfig { configPath = dotenvPaths }
  case toCmd (#input @= args <: r) of
    Just PrintVersion             -> hPutBuilder stdout (Version.build version)
    Just PrintHelp                -> hPutBuilder stdout (fromString usage)
    Just (FetchRawTpl path opts') -> fetchRawTpl path opts'
    Just (FetchTplList opts')     -> fetchTplList opts'
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
      hPutBuilder stdout (fromString $ "undefined subcommand: " <> show args)
      exitFailure
