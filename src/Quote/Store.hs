{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Quote.Store (
    Store (..)
  , initialise
  , get
  , list
  , random
  , add

  , StoreError (..)
  , renderStoreError
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.IORef (IORef)
import           Data.Text (Text)

import           Traction.Control

import           System.IO (IO)

import           Quote.Data
import           Quote.Prelude
import qualified Quote.Store.Memory as Memory
import qualified Quote.Store.Postgres as Postgres

data StoreError =
    PostgresBackendError DbError
    deriving (Show)

renderStoreError :: StoreError -> Text
renderStoreError err =
  case err of
    PostgresBackendError e ->
      mconcat ["Postgres specific backend error: ", renderDbError e]

data Store =
    PostgresStore DbPool
  | MemoryStore (IORef [Quote])

db :: DbPool -> Db a -> EitherT StoreError IO a
db pool =
  firstEitherT PostgresBackendError . runDb pool

initialise :: Store -> EitherT StoreError IO ()
initialise s =
  case s of
    PostgresStore pool ->
      db pool $ Postgres.initialise
    MemoryStore _ ->
      pure ()

get :: Store -> QuoteId -> EitherT StoreError IO (Maybe Quote)
get s n =
  case s of
    PostgresStore pool ->
      db pool $ Postgres.get n
    MemoryStore ref -> do
      liftIO $ Memory.get ref n

list :: Store -> EitherT StoreError IO [Quote]
list s =
  case s of
    PostgresStore pool ->
      db pool $ Postgres.list
    MemoryStore ref -> do
      liftIO $ Memory.list ref

random :: Store -> EitherT StoreError IO (Maybe Quote)
random s =
  case s of
    PostgresStore pool ->
      db pool $ Postgres.random
    MemoryStore ref ->
      liftIO $ Memory.random ref

add :: Store -> Author -> Quote -> EitherT StoreError IO QuoteId
add s author quote =
  case s of
    PostgresStore pool ->
      db pool $ Postgres.add author quote
    MemoryStore ref ->
      liftIO $ Memory.add ref author quote
