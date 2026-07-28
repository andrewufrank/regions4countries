module AggregateSpec (tests) where

import Database.SQLite.Simple
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Aggregate
import R4C.Import.Database
-- import BaseTest.Indicator
-- import R4C.Region3
import R4C.Model
import Study.Dataset
import Study.Region2 
import BaseTest.Region

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
        [ Observation (CountryId "USA") (dsIndicator population) (Year 2024) (Value 100)
        , Observation (CountryId "CAN") (dsIndicator population) (Year 2024) (Value 10)
        , Observation (CountryId "DEU") (dsIndicator population) (Year 2024) (Value 20)
        , Observation (CountryId "AUT") (dsIndicator population) (Year 2024) (Value 999)
        ]

    result1 <- aggregate  regionMembers conn population (Year 2024) -- (RegionId "G7")
    let result = show . colValues $ result1
    close conn

    result @?= "[TerryValue {tvCode = RegionId \"G7\", tvValue = Just 130.0},TerryValue {tvCode = RegionId \"EU\", tvValue = Just 1019.0},TerryValue {tvCode = RegionId \"RUSSIA\", tvValue = Nothing}]"
    
    -- "[TerryValue {tvCode = RegionId \"G7\", tvValue = Just 130.0},TerryValue {tvCode = RegionId \"EU\", tvValue = Just 1019.0}]"


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