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

import           Paths_stack_template_collector (version)
import           RIO
import qualified RIO.ByteString                 as B

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version                   (Version)
import qualified Data.Version                   as Version
import           Development.GitRev
import           StackTemplate.Collector.Cmd
import           System.Exit                    (exitFailure)

main :: IO ()
main = withGetOpt "[options] [show filename]" opts $ \r args ->
  case toCmd (#input @= args <: r) of
    PrintVersion              -> putStrLn $ showVersion version
    FetchRawHsfiles txt opts' -> fetchRawHsfiles txt opts'
    FetchAllHsfiles opts'     -> fetchAllHsfiles opts'
    MistakeCmd                -> mistake args
  where
    opts = #version @= versionOpt
        <: #verbose @= verboseOpt
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
