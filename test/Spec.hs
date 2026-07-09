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
    testGroup "Regions4Countries"
      [ WorldBankSpec.tests
      , AggregateSpec.tests
      , RegionSpec.tests
      , OrchestratorSpec.tests
    --   , IndicatorSpec.tests
    --   , DatabaseSpec.tests
      ]