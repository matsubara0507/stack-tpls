{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module StackTemplates.Cmd.Options where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt

type Options = Record
  '[ "input"   >: [String]
   , "version" >: Bool
   , "verbose" >: Bool
   , "list"    >: Bool
   , "link"    >: Bool
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

listOpt :: OptDescr' Bool
listOpt = optFlag ['l'] ["list"] "List the templates from GitHub"

linkOpt :: OptDescr' Bool
linkOpt = optFlag [] ["link"] "Show link of any stack template"
