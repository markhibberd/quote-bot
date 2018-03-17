{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Quote.Store.Memory (
    get
  , list
  , random
  , add
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.List as List

import           System.IO (IO)
import qualified System.Random.MWC as Random

import           Quote.Data
import           Quote.Prelude

get :: IORef [Quote] -> QuoteId -> IO (Maybe Quote)
get ref n = do
  quotes <- liftIO $ IORef.readIORef ref
  pure . listToMaybe . List.drop (getQuoteId n) $ quotes

list :: IORef [Quote] -> IO [Quote]
list ref =
  liftIO $ IORef.readIORef ref

random :: IORef [Quote] -> IO (Maybe Quote)
random ref = do
  quotes <- IORef.readIORef ref
  Random.withSystemRandom $ \gen -> do
    n <- (Random.uniformR (0, length quotes - 1) gen) :: IO Int
    pure $ listToMaybe . List.drop n $ quotes

add :: IORef [Quote] -> Author -> Quote -> IO QuoteId
add ref _author quote =
  liftIO $ IORef.atomicModifyIORef' ref $ \quotes ->
    (quotes <> [quote], QuoteId $ List.length quotes)
