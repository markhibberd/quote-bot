{-# LANGUAGE NoImplicitPrelude #-}

import           Quote.Prelude

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Quote
import qualified Test.Quote.Store.Postgres


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.Quote.tests
    , Test.Quote.Store.Postgres.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
