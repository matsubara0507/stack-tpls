module StackTemplates.Data.Hsfiles where

import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as Text

import           Data.Extensible
import           Network.HTTP.Req              (https, (/:))
import qualified Network.HTTP.Req              as Req
import qualified StackTemplates.GitHub.GraphQL as GitHub

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

readMaybeDomain :: String -> Maybe Domain
readMaybeDomain "github"    = Just GitHub
readMaybeDomain "gitlab"    = Just GitLab
readMaybeDomain "bitbucket" = Just BitBucket
readMaybeDomain _           = Nothing

domains :: [Domain]
domains = [ GitHub, GitLab, BitBucket ]

readMaybeHsfiles :: String -> Maybe Hsfiles
readMaybeHsfiles str =
  if validateHsfiles owner name domain then Just file else Nothing
  where
    (domain, str') = L.span (/= ':') str
    (owner, name)  = L.span (/= '/') str'
    file = #name   @= fromString (dropWhile (== '/') name)
        <: #owner  @= fromString (dropWhile (== ':') owner)
        <: #domain @= fromMaybe GitHub (readMaybeDomain domain)
        <: nil

validateHsfiles :: String -> String -> String -> Bool
validateHsfiles owner name domain =
  case (owner, name) of
    (':':_:_, '/':_:_) -> domain `elem` map show domains
    _                  -> False

fromRepository :: Domain -> GitHub.Repository -> [Hsfiles]
fromRepository domain repo = files <&> \file ->
     #name   @= GitHub.getTreeName file
  <: #owner  @= GitHub.getOwner repo
  <: #domain @= domain
  <: nil
  where
    files = filter isHsfiles $ maybe [] GitHub.unTreeEntry (repo ^. #object)

isHsfiles :: GitHub.TreeEntry -> Bool
isHsfiles ent = GitHub.isBlob ent && GitHub.hasExt ".hsfiles" ent

toStackArg :: Hsfiles -> Text
toStackArg file = mconcat
  [ tshow (file ^. #domain), ":", file ^. #owner, "/", file ^. #name ]

toRawUrl :: Hsfiles -> Text
toRawUrl file = Text.intercalate "/" $
  case file ^. #domain of
    GitHub    -> [ "https://raw.githubusercontent.com", file ^. #owner, "stack-templates/master", file ^. #name ]
    GitLab    -> [ "https://gitlab.com", file ^. #owner, "stack-templates/raw/master", file ^. #name ]
    BitBucket -> [ "https://bitbucket.org", file ^. #owner, "stack-templates/raw/master", file ^. #name ]

toRawUrl' :: Hsfiles -> Req.Url 'Req.Https
toRawUrl' file =
  case file ^. #domain of
    GitHub    -> https "raw.githubusercontent.com" /: (file ^. #owner) /: "stack-templates" /: "master" /: (file ^. #name)
    GitLab    -> https "gitlab.com" /: (file ^. #owner) /: "stack-templates" /: "raw" /: "master" /: (file ^. #name)
    BitBucket -> https "bitbucket.org" /: (file ^. #owner) /: "stack-templates" /: "raw" /: "master" /: (file ^. #name)

toUrl :: Hsfiles -> Text
toUrl file = Text.intercalate "/" $
  case file ^. #domain of
    GitHub    -> [ "https://github.com", file ^. #owner, "stack-templates/blob/master", file ^. #name ]
    GitLab    -> [ "https://gitlab.com", file ^. #owner, "stack-templates/blob/master", file ^. #name ]
    BitBucket -> [ "https://bitbucket.org", file ^. #owner, "stack-templates/src/master", file ^. #name ]
