{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplate.Collector.Data.GitObject where

import           RIO

import           Data.Extensible

type Commit = Record
   '[ "tree" >: Tree
    ]

type Tree = Record
   '[ "entries" >: [TreeEntry]
    ]

type TreeEntry = Record
   '[ "name" >: Text
    , "type" >: Text
    ]
