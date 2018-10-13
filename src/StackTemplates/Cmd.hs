{-# LANGUAGE OverloadedLabels #-}

module StackTemplates.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           StackTemplates.Cmd.Options as X
import           StackTemplates.Cmd.Run     as X

data Cmd
  = PrintVersion
  | FetchTplList Options
  | FetchRawTpl Text Options
  deriving (Show, Eq)

toCmd :: Options -> Maybe Cmd
toCmd opts
  | opts ^. #version = Just PrintVersion
  | opts ^. #list    = Just $ FetchTplList opts
  | otherwise        = flip FetchRawTpl opts <$> path
  where
    path = fromString <$> listToMaybe (opts ^. #input)
