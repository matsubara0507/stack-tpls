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
  | FetchRawHsfiles Text Options
  | FetchAllHsfiles Options
  | MistakeCmd
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version      = PrintVersion
  | isJust txt            = FetchRawHsfiles (fromMaybe "" txt) opts
  | null (opts ^. #input) = FetchAllHsfiles opts
  | otherwise             = MistakeCmd
  where
    txt = getShowCmdName opts

getShowCmdName :: Options -> Maybe Text
getShowCmdName opts =
  case opts ^. #input of
    [ "show", txt ] -> Just (fromString txt)
    _               -> Nothing
