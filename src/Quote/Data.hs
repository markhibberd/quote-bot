{-# LANGUAGE NoImplicitPrelude #-}
module Quote.Data (
    QuoteId (..)
  , Quote (..)
  , Author (..)
  ) where

import           Data.Text (Text)

import           Quote.Prelude


newtype Quote =
  Quote {
      getQuote :: Text
    } deriving (Eq, Ord, Show)

newtype QuoteId =
  QuoteId {
      getQuoteId :: Int
    } deriving (Eq, Ord, Show)

newtype Author =
  Author {
      getAuthor :: Text
    } deriving (Eq, Ord, Show)
