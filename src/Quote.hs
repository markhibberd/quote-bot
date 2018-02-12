{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Quote (
    Quote (..)
  , quote
  , readQuotes
  ) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Network.Linklater as Linklater

import           System.IO (IO)
import qualified System.IO as IO
import qualified System.Random.MWC as Random

import           Quote.Prelude

newtype Quote =
  Quote {
      getQuote :: Text
    } deriving (Eq, Ord, Show)

readQuotes :: IO.FilePath -> IO [Quote]
readQuotes path = do
  content <- Text.readFile path
  pure $ Quote <$> Text.splitOn "\n" content

quote :: [Quote] -> Linklater.Config -> Linklater.Command -> IO Text
quote quotes config (Linklater.Command _name _user channel _text) = do
  q <- Random.withSystemRandom $ \gen -> do
    n <- (Random.uniformR (0, length quotes - 2) gen) :: IO Int
    pure $ List.head . List.drop n $ quotes
  let
    message =
      Linklater.SimpleMessage
        (Linklater.EmojiIcon ":8ball:")
        "quotes"
        channel
        ("> " <> getQuote q)

  e <- runEitherT $ Linklater.say message config
  case e of
    Left err -> do
      IO.print err
    Right _ -> do
      pure ()
  pure ""
