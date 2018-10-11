{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplate.Collector.Data.PageInfo where

import           RIO

import           Data.Extensible

type PageInfo = Record
   '[ "endCursor"   >: Text
    , "hasNextPage" >: Bool
    ]
