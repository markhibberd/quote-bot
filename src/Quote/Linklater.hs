{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Quote.Linklater (
    slashMessage
  , ResponseMessage (..)
  , ResponseType (..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Aeson (object, (.=))

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Network.HTTP.Types as Http
import qualified Network.Linklater as Linklater
import qualified Network.Linklater.Types as Linklater
import qualified Network.Wai as Wai

import           System.IO (IO)

import           Quote.Prelude

slashMessage :: (Linklater.Command -> IO ResponseMessage) -> Wai.Application
slashMessage f =
  Linklater.slash $ \command _ respond ->
    f command >>=
      respond . Wai.responseLBS Http.status200 [("Content-Type", "application/json")] .
        Aeson.encode

data ResponseMessage =
    ResponseSimpleMessage
      !Linklater.Icon
      !Text
      !Linklater.Channel
      !ResponseType
      !Text
  | ResponseFormattedMessage
      !Linklater.Icon
      !Text
      !Linklater.Channel
      !ResponseType
      ![Linklater.Format]

data ResponseType =
    InChannel
  | Ephemeral

instance Aeson.ToJSON ResponseMessage where
   toJSON m =
     case m of
       (ResponseFormattedMessage emoji username channel response formats) ->
         toJSON_ emoji username channel response (Text.unwords (Linklater.unformat <$> formats)) False
       (ResponseSimpleMessage emoji username channel response text) ->
         toJSON_ emoji username channel response text True


toJSON_ :: Linklater.Icon -> Text -> Linklater.Channel -> ResponseType -> Text -> Bool -> Aeson.Value
toJSON_ (Linklater.EmojiIcon emoji) username channel response raw toParse =
  object [
      "channel" .= channel
    , "icon_emoji" .= (":" <> emoji <> ":")
    , "parse" .= ((if toParse then "full" else "poop") :: Text)
    , "username" .= username
    , "text" .= raw
    , "unfurl_links" .= True
    , "response_type" .= (case response of
        InChannel ->
          "in_channel" :: Text
        Ephemeral ->
          "ephemeral" :: Text)
    ]
