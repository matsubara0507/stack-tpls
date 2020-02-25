module StackTemplates.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    , Env
    ) where

import           RIO

import           StackTemplates.Cmd.Options as X
import           StackTemplates.Cmd.Run     as X
import           StackTemplates.Env         (Env)

data Cmd
  = PrintVersion
  | PrintHelp
  | FetchTplList Options
  | FetchRawTpl Text Options
  deriving (Show, Eq)

toCmd :: Options -> Maybe Cmd
toCmd opts
  | opts ^. #version = Just PrintVersion
  | opts ^. #help    = Just PrintHelp
  | opts ^. #list    = Just $ FetchTplList opts
  | otherwise        = flip FetchRawTpl opts <$> path
  where
    path = fromString <$> listToMaybe (opts ^. #input)
