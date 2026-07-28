-----------------------------------------------------------------------------
--
-- Module      :   Test
-----------------------------------------------------------------------------

module Main where

import Test.Tasty

import qualified AggregateSpec
import qualified DatasetSyncSpec
import qualified MarkdownSpec
-- import qualified DatabaseSpec
-- import qualified IndicatorSpec
import qualified RegionSpec
import qualified WorldBankSpec
import qualified OrchestratorSpec
import qualified CountrySpec 
main :: IO ()
main =
  defaultMain $
    testGroup "Spec"
      [ WorldBankSpec.tests
      , RegionSpec.tests
      , OrchestratorSpec.tests
      , AggregateSpec.tests
      , DatasetSyncSpec.tests
      , MarkdownSpec.tests
      , CountrySpec.tests
    --   , IndicatorSpec.tests
    --   , DatabaseSpec.tests
      ]
