{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module StackTemplates.GitHub.GraphQL
  ( module X
  , searchRepository
  ) where

import           RIO

import           Data.Morpheus.Client                     (fetch)
import           Network.HTTP.Req
import qualified StackTemplates.GitHub.GraphQL.Query      as Query
import           StackTemplates.GitHub.GraphQL.Repository as X

resolver :: MonadIO m => ByteString -> LByteString -> m LByteString
resolver tok b = runReq defaultHttpConfig $ do
    let headers = header "Content-Type" "application/json"
               <> header "User-Agent" "github-grahql-example.hs"
               <> oAuth2Bearer tok
    responseBody <$> req POST (https "api.github.com" /: "graphql") (ReqBodyLbs b) lbsResponse headers

searchRepository :: MonadIO m => ByteString -> Text -> m (Either String [Repository])
searchRepository tok query = go [] (Query.SearchPageInfoPageInfo Nothing True)
  where
    fetch' = fetch (resolver tok)
    go xs (Query.SearchPageInfoPageInfo _ False) = pure (Right xs)
    go xs (Query.SearchPageInfoPageInfo cursor _) = do
      result <- fetch' Query.SearchRepositoryArgs {..}
      case result of
        Left e ->
          pure $ Left (show e)
        Right (Query.SearchRepository (Query.SearchSearchResultItemConnection _ next edges)) ->
          let repos = maybe [] (mapMaybe toRepositoryInfo . catMaybes) edges in
          go (repos ++ xs) next
