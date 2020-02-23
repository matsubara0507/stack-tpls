module StackTemplates.Data.PageInfo where

import           RIO

import           Data.Extensible

type PageInfo = Record
   '[ "endCursor"   >: Maybe Text
    , "hasNextPage" >: Bool
    ]
