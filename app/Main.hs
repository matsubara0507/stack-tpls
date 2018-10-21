{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_stack_tpls       (version)
import           RIO
import qualified RIO.ByteString         as B
import           RIO.Directory

import           Configuration.Dotenv   (Config (..), defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version           (Version)
import qualified Data.Version           as Version
import           Development.GitRev
import           StackTemplates.Cmd
import           System.Exit            (exitFailure)

main :: IO ()
main = withGetOpt "[options] [show filename]" opts $ \r args -> do
  homeDir <- getHomeDirectory
  dotenvPaths <- filterM doesFileExist [".env", homeDir ++ "/.env"]
  _ <- loadFile $ defaultConfig { configPath = dotenvPaths }
  case toCmd (#input @= args <: r) of
    Just PrintVersion             -> putStrLn $ showVersion version
    Just (FetchRawTpl path opts') -> fetchRawTpl path opts'
    Just (FetchTplList opts')     -> fetchTplList opts'
    Nothing                       -> mistake args
  where
    opts = #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #list    @= listOpt
        <: #link    @= linkOpt
        <: nil
    mistake args = do
      putStrLn $ "undefined subcommand: " <> show args
      exitFailure

putStrLn :: MonadIO m => String -> m ()
putStrLn str = B.putStr $ fromString (str <> "\n")

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
