{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Quote (
    quote

  , QuoteError (..)
  , renderQuoteError
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Network.Linklater as Linklater
import qualified Network.Linklater.Types as Linklater

import           System.IO (IO)

import           Quote.Data
import qualified Quote.Linklater as Linklater
import           Quote.Prelude
import           Quote.Store (Store (..), StoreError (..))
import qualified Quote.Store as Store

data QuoteError =
    QuoteStoreError StoreError
  | QuoteSayError Linklater.RequestError
    deriving (Show)

renderQuoteError :: QuoteError -> Text
renderQuoteError err =
  case err of
    QuoteStoreError e ->
      mconcat ["Store error / ", Store.renderStoreError e]
    QuoteSayError e ->
      mconcat ["Request error / ", Text.pack . show $ e]

quote :: Store -> Linklater.Config -> Linklater.Command -> EitherT QuoteError IO Linklater.ResponseMessage
quote store _config (Linklater.Command name (Linklater.User user) channel text) = do
  case name of
    "quote" -> do
      case (text >>= fmap QuoteId . readMaybe . Text.unpack . Text.strip) of
        Nothing ->
          random store channel
        Just identifier ->
          get store identifier channel
    "add-quote" -> do
      identifier <- firstEitherT QuoteStoreError $
        Store.add store (Author user) (Quote . Text.strip $ fromMaybe "" text)
      pure $
        Linklater.ResponseSimpleMessage
          (Linklater.EmojiIcon ":8ball:")
          "quotes"
          channel
          Linklater.InChannel
          (mconcat ["quote added [", Text.pack . show . getQuoteId $ identifier, "]"])

    _ ->
      pure $
        Linklater.ResponseSimpleMessage
          (Linklater.EmojiIcon ":8ball:")
          "quotes"
          channel
          Linklater.InChannel
          (mconcat ["usage: /quote"])

random :: Store -> Linklater.Channel -> EitherT QuoteError IO Linklater.ResponseMessage
random store channel = do
  q <- firstEitherT QuoteStoreError $
    Store.random store
  pure $
    Linklater.ResponseSimpleMessage
      (Linklater.EmojiIcon ":8ball:")
      "quotes"
      channel
      Linklater.InChannel
     (mconcat ["> ", fromMaybe "There really should be a witty quote here, but I couldn't find one for some reason - quotebot" $ getQuote <$> q])

get :: Store -> QuoteId -> Linklater.Channel -> EitherT QuoteError IO Linklater.ResponseMessage
get store identifier channel = do
  q <- firstEitherT QuoteStoreError $
    Store.get store identifier
  pure $
    Linklater.ResponseSimpleMessage
      (Linklater.EmojiIcon ":8ball:")
      "quotes"
      channel
      Linklater.InChannel
     (mconcat ["> ", fromMaybe "There really should be a witty quote here, but I couldn't find one for some reason - quotebot" $ getQuote <$> q])
