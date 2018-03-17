{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Quote.Store.Postgres where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (hoist, lift)

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           System.IO (IO)

import           Traction.Control

import           Quote.Data
import           Quote.Prelude
import qualified Quote.Store.Postgres as Postgres


prop_db :: Property
prop_db = property $ do
  quote <- forAll $ Quote <$> Gen.element ["red", "green", "blue"]
  author <- forAll $ Author <$> Gen.element ["fred", "wilma", "barney", "betty"]
  result <- db $ do
    q <- Postgres.add author quote
    list <- Postgres.list
    one <- Postgres.get q
    random <- Postgres.random
    pure $ (list, one, random)
  ([quote], Just quote, Just quote) === result

db :: Db a -> PropertyT IO a
db x = do
  pool <- mkPool
  evalExceptT . hoist lift . testDb pool $ Postgres.initialise >> x

checkDb :: MonadIO m => Group -> m Bool
checkDb group =
  case group of
    Group name properties ->
      checkSequential (Group name ((fmap . fmap) (withTests 5) properties))

mkPool :: MonadIO m => m DbPool
mkPool =
  liftIO $ newPool "dbname=quote_test host=localhost user=quote_test password=quote_test port=5432"

tests :: IO Bool
tests =
  checkDb $$(discover)
