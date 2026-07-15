-----------------------------------------------------------------------------
--
-- Module      :   Test
-----------------------------------------------------------------------------

module Main where

import Test.Tasty

import qualified AggregateSpec
-- import qualified DatabaseSpec
-- import qualified IndicatorSpec
import qualified RegionSpec
import qualified WorldBankSpec
import qualified OrchestratorSpec

main :: IO ()
main =
  defaultMain $
    testGroup "Spec"
      [ WorldBankSpec.tests
      , RegionSpec.tests
      , OrchestratorSpec.tests
      , AggregateSpec.tests
    --   , IndicatorSpec.tests
    --   , DatabaseSpec.tests
      ]