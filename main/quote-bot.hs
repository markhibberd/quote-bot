{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Nest

import qualified Network.Linklater as Linklater

import           System.IO (IO)
import qualified System.IO as IO

import qualified Quote
import           Quote.Prelude

import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  port <- Nest.force $ Nest.numeric "PORT" `Nest.withDefault` 8888
  hook <- Nest.force $ Nest.string "HOOK"
  file <- Nest.force $ Nest.string "QUOTES"
  IO.putStrLn ("[quote-bot] listening on port: " <> show port)
  quotes <- Quote.readQuotes file
  Warp.run port (Linklater.slashSimple $ Quote.quote quotes (Linklater.Config hook))
