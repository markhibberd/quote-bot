{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Quote.Store.Postgres (
    initialise
  , get
  , list
  , random
  , add
  ) where

import           Traction.Control
import           Traction.Migration (Migration (..))
import           Traction.QQ (sql)
import qualified Traction.Migration as Traction
import qualified Traction.Sql as Traction

import           Quote.Data
import           Quote.Prelude

initialise :: MonadDb m => m ()
initialise =
  void . liftDb $ Traction.migrate schema

get :: MonadDb m => QuoteId -> m (Maybe Quote)
get n =
  (fmap . fmap) Quote . Traction.values $ Traction.unique [sql|
      SELECT quote
        FROM quote
       WHERE id = ?
    |] (Traction.Only $ getQuoteId n)

list :: MonadDb m => m [Quote]
list =
  (fmap . fmap) Quote . Traction.values $ Traction.query_ [sql|
      SELECT quote
        FROM quote
    |]

random :: MonadDb m => m (Maybe Quote)
random =
  (fmap . fmap) Quote . Traction.values $ Traction.unique_ [sql|
        SELECT quote
          FROM quote
         ORDER BY RANDOM()
         LIMIT 1
      |]

add :: MonadDb m => Author -> Quote -> m QuoteId
add author quote =
  fmap QuoteId . Traction.value $ Traction.mandatory [sql|
      INSERT INTO quote (quote, author)
           VALUES (?, ?)
        RETURNING id
    |] (getQuote quote, getAuthor author)

schema :: [Migration]
schema = [
    Migration "create-quote" [sql|
      CREATE TABLE quote (
          id SERIAL PRIMARY KEY
        , quote TEXT NOT NULL
        , author TEXT
        , created_at TIMESTAMP WITH TIME ZONE DEFAULT now()
        )
    |]
  ]
