{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Nest

import qualified Network.Linklater as Linklater

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Traction.Control as Traction

import qualified Quote
import           Quote.Data
import qualified Quote.Linklater as Linklater
import           Quote.Prelude
import           Quote.Store (Store (..))
import qualified Quote.Store as Store

import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  port <- Nest.force $ Nest.numeric "PORT" `Nest.withDefault` 8888
  hook <- Nest.force $ Nest.string "HOOK"
  store <- Nest.force $ bootstrap
  runEitherT (Store.initialise store) >>= \e -> case e of
    Right _ ->
      pure ()
    Left err -> do
      Text.hPutStrLn IO.stderr $ Store.renderStoreError err
      Exit.exitFailure
  IO.putStrLn ("[quote-bot] listening on port: " <> show port)
  Warp.run port $ (Linklater.slashMessage . wrap $
    (Quote.quote store (Linklater.Config hook)))

bootstrap :: MonadIO m => Nest.Parser m Store
bootstrap =
  join $ Nest.setting "BOT_MODE" (Map.fromList [
      ("db", db)
    , ("file", file)
    ]) `Nest.withDefault` file

db :: MonadIO m => Nest.Parser m Store
db = do
  conn <- Nest.string "DB"
  pool <- liftIO $ Traction.newPool conn
  pure $ PostgresStore pool

file :: MonadIO m => Nest.Parser m Store
file = do
  path <- Nest.string "QUOTES"
  content <- liftIO $ Text.readFile path
  fmap MemoryStore . liftIO . IORef.newIORef $
    Quote <$> Text.splitOn "\n" content

wrap :: (Linklater.Command -> EitherT Quote.QuoteError IO Linklater.ResponseMessage) -> Linklater.Command -> IO Linklater.ResponseMessage
wrap f a@(Linklater.Command _ _ channel _) =
  runEitherT (f a) >>= \e -> case e of
    Left err -> do
      Text.putStrLn . Quote.renderQuoteError $ err
      pure $
        Linklater.ResponseSimpleMessage
          (Linklater.EmojiIcon ":8ball:")
          "quotes"
          channel
          Linklater.Ephemeral
          (Quote.renderQuoteError err)
    Right t ->
      pure t
