{-# LANGUAGE OverloadedLabels #-}

module StackTemplate.Collector.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           StackTemplate.Collector.Cmd.Options as X
import           StackTemplate.Collector.Cmd.Run     as X

data Cmd
  = PrintVersion
  | FetchHsfiles Options
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | otherwise        = FetchHsfiles opts
