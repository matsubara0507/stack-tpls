module StackTemplates.Data.PageInfo where

import           RIO

import           Data.Extensible

type PageInfo = Record
   '[ "endCursor"   >: Text
    , "hasNextPage" >: Bool
    ]
