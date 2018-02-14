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



quote :: Store -> Linklater.Config -> Linklater.Command -> EitherT QuoteError IO Text
quote store config (Linklater.Command name _user channel _text) = do
  case name of
    "quote" -> do
      q <- firstEitherT QuoteStoreError $
        Store.random store
      let
        message =
          Linklater.SimpleMessage
            (Linklater.EmojiIcon ":8ball:")
            "quotes"
            channel
            (mconcat ["> ", fromMaybe "There really should be a witty quote here, but I couldn't find one for some reason - quotebot" $ getQuote <$> q])
      firstEitherT QuoteSayError $
        Linklater.say message config
      pure ""
    _ ->
      pure "usage: /quote"
