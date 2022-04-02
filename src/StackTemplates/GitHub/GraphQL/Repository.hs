

module StackTemplates.GitHub.GraphQL.Repository where

import           RIO
import qualified RIO.Text                            as Text

import           Data.Extensible
import qualified StackTemplates.GitHub.GraphQL.Query as Query

type Repository = Record
   '[ "name"          >: Text
    , "nameWithOwner" >: Text
    , "object"        >: Maybe Commit
    ]

newtype Commit = Commit { unTreeEntry :: [TreeEntry] } deriving (Eq, Show)

type TreeEntry = Query.SearchEdgesNodeObjectTreeEntriesTreeEntry

getTreeName :: TreeEntry -> Text
getTreeName (Query.SearchEdgesNodeObjectTreeEntriesTreeEntry name _) = name

isBlob :: TreeEntry -> Bool
isBlob (Query.SearchEdgesNodeObjectTreeEntriesTreeEntry _ ty) = ty == "blob"

hasExt :: Text -> TreeEntry -> Bool
hasExt ext (Query.SearchEdgesNodeObjectTreeEntriesTreeEntry name _) = ext `Text.isSuffixOf` name

getOwner :: Repository -> Text
getOwner = Text.takeWhile (/= '/') . view #nameWithOwner

toRepositoryInfo :: Query.SearchEdgesSearchResultItemEdge -> Maybe Repository
toRepositoryInfo (Query.SearchEdgesSearchResultItemEdge (Just (Query.SearchEdgesNodeRepository _ nameWithOwner name obj))) =
  Just $ #name @= name <: #nameWithOwner @= nameWithOwner <: #object @= object <: nil
  where
    object = case obj of
      (Just (Query.SearchEdgesNodeObjectCommit _ (Query.SearchEdgesNodeObjectTreeTree entries))) ->
        Just $ Commit (fromMaybe [] entries)
      _ ->
        Nothing
toRepositoryInfo _ = Nothing

