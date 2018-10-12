{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplate.Collector.Data.Hsfiles where

import           RIO
import qualified RIO.Text                                as Text

import           Data.Extensible
import           StackTemplate.Collector.Data.GitObject  (TreeEntry, isBlob)
import           StackTemplate.Collector.Data.Repository (Repository, getOwner)

type Hsfiles = Record
   '[ "name"   >: Text
    , "owner"  >: Text
    , "domain" >: Domain
    ]

data Domain
  = GitHub
  | GitLab
  | BitBucket
  deriving (Eq)

instance Show Domain where
  show GitHub    = "github"
  show GitLab    = "gitlab"
  show BitBucket = "bitbucket"

fromRepository :: Domain -> Repository -> [Hsfiles]
fromRepository domain repo = flip map files $ \file ->
     #name   @= (file ^. #name)
  <: #owner  @= getOwner repo
  <: #domain @= domain
  <: nil
  where
    files = filter isHsfiles $
      maybe [] (view #entries . view #tree) (repo ^. #object)

isHsfiles :: TreeEntry -> Bool
isHsfiles ent = isBlob ent && (Text.isSuffixOf ".hsfiles" $ ent ^. #name)

toStackArg :: Hsfiles -> Text
toStackArg file = mconcat
  [ tshow (file ^. #domain), ":", file ^. #owner, "/", file ^. #name ]
