{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module StackTemplates.Env where

import           RIO

import           Data.Extensible

type Env = Record
  '[ "logger"   >: LogFunc
   , "gh_token" >: ByteString
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
