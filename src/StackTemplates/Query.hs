{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module StackTemplates.Query where

import           RIO
import qualified RIO.Text                       as Text

import           Data.Extensible
import           StackTemplates.Data.PageInfo   (PageInfo)
import           StackTemplates.Data.Repository (Repository)

type Response = Record '[ "data" >: ResponseData ]

type ResponseData = Record '[ "search" >: SearchResult ]

type SearchResult = Record
  '[ "repositoryCount" >: Int
   , "pageInfo"        >: PageInfo
   , "edges"           >: [SearchResultEdge]
   ]

type SearchResultEdge = Record
  '[ "node" >: Repository
   ]

data SearchType
  = Repository
  | User
  deriving (Eq)

instance Show SearchType where
  show Repository = "REPOSITORY"
  show User       = "USER"

type SearchOpts = Record
   '[ "first" >: Int
    , "after" >: Maybe Text
    ]

searchQuery :: Text -> SearchType -> SearchOpts -> Text
searchQuery query stype opts = mconcat
  [ "query{search("
  , "query:", tshow query, ","
  , "type:", tshow stype, ","
  , toSearchArgsText opts
  , "){", Text.intercalate "," fields, "}}"
  ]
  where
    fields =
      [ "repositoryCount"
      , "pageInfo{ endCursor, hasNextPage }"
      , "edges{ node{ ... on Repository{ nameWithOwner, name, " <> obj <> " }}}"
      ]
    obj = "object(expression:\"master\"){ ... on Commit{ tree{ entries{ name, type }}}}"

toSearchArgsText :: SearchOpts -> Text
toSearchArgsText opts = mconcat
  [ "first:", tshow (opts ^. #first)
  , maybe "" (\txt -> ", after: " <> tshow txt) (opts ^. #after)
  ]
