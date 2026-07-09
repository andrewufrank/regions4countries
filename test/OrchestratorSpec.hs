module OrchestratorSpec (tests) where


import Test.Tasty
import Test.Tasty.HUnit

import Database.SQLite.Simple

import Control.Monad (forM_)

import R4C.Orchestrator
import R4C.WorldBank
import R4C.Database
import R4C.Query 
import R4C.Model
import R4C.Indicator

tests :: TestTree
tests =
  testGroup "Orchestrator"
    [ testCase "import test files and check results" testOrchestrator
    ]

testOrchestrator :: Assertion
testOrchestrator = do
    let db = ":memory"

--   exists <- doesFileExist db
--   if exists then removeFile db else pure ()

    importWorldBankFiles
        db
        [ 
            -- "/home/frank/Desktop/buecher/nextOrder/WorldBankData/population/f27274b4-7384-4c6e-b81d-7ddf2ac9bb9a_Data.csv"
            -- , 
            "/home/frank/Desktop/buecher/nextOrder/WorldBankData/surfaceArea/API_AG.SRF.TOTL.K2_DS2_en_csv_v2_4649.csv"
            ]

    conn <- open db

    surfaceRows <- surfaceAreaAustria conn
    assertBool "Austria surface area should not be empty" $
        not (null surfaceRows)

    averageSurface <-
        averageValue conn
        (CountryId "AUT")
        (IndicatorId "AG.SRF.TOTL.K2")

    averageSurface @?=   83879.0

    close conn


    