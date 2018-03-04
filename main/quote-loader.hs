{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Nest

import qualified System.Environment as Environment
import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Traction.Control as Traction

import           Quote.Data
import           Quote.Prelude
import           Quote.Store (Store (..))
import qualified Quote.Store as Store


main :: IO ()
main = do
  conn <- Nest.force $ Nest.string "DB"
  store <- fmap PostgresStore $ Traction.newPool conn
  Environment.getArgs >>= \args -> case args of
    [path] -> do
      content <- liftIO $ Text.readFile path
      result <- runEitherT . for_ (Quote <$> Text.splitOn "\n" content) $ \quote -> do
        Store.add store (Author "quote-loader") quote
      case result of
        Right _ ->
          Exit.exitSuccess
        Left e -> do
          Text.hPutStrLn IO.stderr $ Store.renderStoreError e
          Exit.exitFailure
    _  -> do
      Text.hPutStrLn IO.stderr "usage: quote-loader QUOTE_FILE"
      Exit.exitFailure
