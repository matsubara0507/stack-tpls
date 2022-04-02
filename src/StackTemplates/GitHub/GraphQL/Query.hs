{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module StackTemplates.GitHub.GraphQL.Query where

import           RIO

import           Data.Morpheus.Client

-- | MEMO: need `__typename` for interface on morpheus-graphql-client-0.18.0
defineByDocumentFile "./assets/schema.docs.graphql"
  [gql|
    query SearchRepository($query: String!, $cursor: String) {
      search(query: $query, type: REPOSITORY, first: 100, after: $cursor) {
        repositoryCount,
        pageInfo { endCursor, hasNextPage }
        edges {
          node {
            ... on Repository {
              __typename
              nameWithOwner,
              name,
              object(expression: "HEAD") {
                ... on Commit { __typename, tree { entries { name, type } } }
              }
            }
          }
        }
      }
    }
  |]
