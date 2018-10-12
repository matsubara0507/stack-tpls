{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplate.Collector.Data.Repository where

import           RIO
import qualified RIO.Text                               as Text

import           Data.Extensible
import           StackTemplate.Collector.Data.GitObject (Commit)

type Repository = Record
   '[ "name"          >: Text
    , "nameWithOwner" >: Text
    , "object"        >: Maybe Commit
    ]

getOwner :: Repository -> Text
getOwner = Text.takeWhile (/= '/') . view #nameWithOwner
