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

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args ->
  case toCmd (#input @= args <: r) of
    PrintVersion       -> B.putStr $ fromString (showVersion version)
    FetchHsfiles opts' -> fetchHsfiles opts'
  where
    opts = #version @= versionOpt
        <: #verbose @= verboseOpt
        <: nil

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
