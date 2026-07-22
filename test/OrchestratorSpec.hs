module OrchestratorSpec (tests) where


import Test.Tasty
import Test.Tasty.HUnit

import Database.SQLite.Simple
import System.Directory
import Control.Monad (forM_)

import R4C.Import.Orchestrator
import R4C.Import.WorldBank
import R4C.Import.Database
-- import R4C.Query 
import R4C.Model
-- import BaseTest.Indicator

tests :: TestTree
tests =
  testGroup "Orchestrator"
    [ testCase "import test files and check results" testOrchestrator
    ]

testOrchestrator :: Assertion
testOrchestrator = do
    let db = "test4.sqlite"

    exists <- doesFileExist db
    if exists then removeFile db else pure ()

    importWorldBankArchives
        db
        [ "test/testdata/API_AG.SRF.TOTL.K2_DS2_en_csv_v2_4649.zip"
            -- "/home/frank/Desktop/buecher/nextOrder/WorldBankData/population/f27274b4-7384-4c6e-b81d-7ddf2ac9bb9a_Data.csv"
            -- , 
            -- "test/testdata/API_AG.SRF.TOTL.K2_DS2_en_csv_v2_4649.csv"
            ]

    conn <- open db
    createSchema conn
    
    surfaceRows <- surfaceAreaAustria conn
    assertBool "Austria surface area should not be empty" $
        not (null surfaceRows)

    averageSurface <-
        averageValue conn
        (CountryId "AUT")
        (IndicatorId "AG.SRF.TOTL.K2")

    averageSurface @?=   83879.0

    close conn



-- testq = do 
--     conn <- open "test.sqlite"
--     obs <- showObservations conn
--     mapM_ print obs 

--     rows <- surfaceAreaAustria conn
--     mapM_ print rows

--     avg <- averageValue conn (CountryId "AUT") (IndicatorId "AG.SRF.TOTL.K2")
--     print avg 

--     close conn

showObservations conn = do
    rows :: [Observation] <- query_ conn
        "SELECT country, indicator, year, value \
        \FROM observation \
        \LIMIT 10"
    pure rows

surfaceAreaAustria :: Connection -> IO [YearValue]
surfaceAreaAustria conn = do
    rows :: [YearValue] <- query conn
        "SELECT year, value \
        \FROM observation \
        \WHERE country = ? \
        \AND indicator = ? \
        \ORDER BY year"
        (CountryId "AUT", IndicatorId "AG.SRF.TOTL.K2") 
    pure rows 

    

averageValue
    :: Connection
    -> CountryId
    -> IndicatorId
    -> IO Double
averageValue conn country indicator = do

    [Only avg] <- query conn
        "SELECT AVG(value) \
        \FROM observation \
        \WHERE country = ? \
        \AND indicator = ?"
        (country, indicator)

    pure avg
    