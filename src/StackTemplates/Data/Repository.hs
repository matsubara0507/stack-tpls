{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplates.Data.Repository where

import           RIO
import qualified RIO.Text                      as Text

import           Data.Extensible
import           StackTemplates.Data.GitObject (Commit)

type Repository = Record
   '[ "name"          >: Text
    , "nameWithOwner" >: Text
    , "object"        >: Maybe Commit
    ]

getOwner :: Repository -> Text
getOwner = Text.takeWhile (/= '/') . view #nameWithOwner
