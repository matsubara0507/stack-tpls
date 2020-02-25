module StackTemplates.Data.GitObject where

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

isBlob :: TreeEntry -> Bool
isBlob ent = ent ^. #type == "blob"
