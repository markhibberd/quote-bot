{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Quote where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.IO (IO)

import           Quote.Prelude


prop_example :: Property
prop_example = property $ do
  value <- forAll $ Gen.int (Range.constant 0 999999)
  value === value

tests :: IO Bool
tests =
  pure True
--  checkParallel $$(discover)
