-----------------------------------------------------------------------------
--
-- Module      :   Test
-----------------------------------------------------------------------------

module Main where

import Test.Tasty

import qualified AggregateSpec
-- import qualified DatasetSyncSpec
import qualified MarkdownSpec
-- import qualified DatabaseSpec
-- import qualified IndicatorSpec
import qualified RegionSpec
import qualified WorldBankSpec
import qualified OrchestratorSpec
import qualified Tab99spec 
import qualified Tab98spec 
main :: IO ()
main =
  defaultMain $
    testGroup "Spec"
      [ WorldBankSpec.tests
      , RegionSpec.tests
      , OrchestratorSpec.tests
      , AggregateSpec.tests
      -- , DatasetSyncSpec.tests
      , MarkdownSpec.tests
      , Tab99spec.tests
      , Tab98spec.tests
    --   , IndicatorSpec.tests
    --   , DatabaseSpec.tests
      ]
