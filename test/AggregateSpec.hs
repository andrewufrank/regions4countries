module AggregateSpec (tests) where

import Database.SQLite.Simple
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Aggregate
import R4C.Database
import BaseTest.Indicator
import R4C.Model

tests :: TestTree
tests =
    testGroup "Aggregate"
        [ testCase "aggregate sums G7 population" testAggregateG7
        ]

testAggregateG7 :: Assertion
testAggregateG7 = do
    conn <- open ":memory:"
    createSchema conn

    insertObservations conn
        [ Observation (CountryId "USA") (indicatorId population) (Year 2024) (Value 100)
        , Observation (CountryId "CAN") (indicatorId population) (Year 2024) (Value 10)
        , Observation (CountryId "DEU") (indicatorId population) (Year 2024) (Value 20)
        , Observation (CountryId "AUT") (indicatorId population) (Year 2024) (Value 999)
        ]

    result <- aggregate conn population (Year 2024) (RegionId "G7")

    close conn

    result @?= Just 130


-- testPop = do 
--     conn <- open "test.sqlite"
--     pops <- mapM (showAggregate conn population (Year 2024)) regions2
--     mapM_ print $ zip regions2 pops 
--     close conn

-- testa = do 
--     conn <- open "test.sqlite"
   
--     -- showAggregate conn population (Year 2024) (RegionId "EU")
--     -- showAggregate conn population (Year 2024) (RegionId "G7")
--     showAggregate conn population (Year 2024) (RegionId "GULF")
--     showAggregate conn population (Year 2024) (RegionId "RUSSIA")
    
--     close conn   