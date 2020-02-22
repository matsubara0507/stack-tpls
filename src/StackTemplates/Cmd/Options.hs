module StackTemplates.Cmd.Options where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt

type Options = Record
  '[ "input"   >: [String]
   , "version" >: Bool
   , "help"    >: Bool
   , "verbose" >: Bool
   , "list"    >: Bool
   , "link"    >: Bool
   , "update"  >: Bool
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show usages"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

listOpt :: OptDescr' Bool
listOpt = optFlag ['l'] ["list"] "List the templates from GitHub"

linkOpt :: OptDescr' Bool
linkOpt = optFlag [] ["link"] "Show link of any stack template"

updateOpt :: OptDescr' Bool
updateOpt = optFlag [] ["update"] "Update stack templates list in local cache"
