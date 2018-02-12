{-# LANGUAGE NoImplicitPrelude #-}

import           Quote.Prelude

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Quote


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.Quote.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
