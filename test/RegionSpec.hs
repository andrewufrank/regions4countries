module RegionSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model
import BaseTest.Region
import R4C.Aggregate

tests :: TestTree
tests =
  testGroup "Region"
    [ testCase "G7 has seven members" $
        length (countriesInRegion regionMembers (RegionId "G7")) @?= 7

    , testCase "Austria is in EU" $
        assertBool "AUT should be in EU" $
          CountryId "AUT" `elem` countriesInRegion regionMembers (RegionId "EU")

    , testCase "Austria is not in G7" $
        assertBool "AUT should not be in G7" $
          CountryId "AUT" `notElem` countriesInRegion regionMembers (RegionId "G7")
    ]

-- testRegions = do
--   conn <- open "test.sqlite"

--   rows <- query_ conn
--     "SELECT region, country FROM country_region ORDER BY region, country LIMIT 50"
--       :: IO [(RegionId, CountryId)]

--   mapM_ print rows
--   close conn

-- now working, regions are not stored
-- debugRegion :: Connection -> IndicatorId -> Year -> RegionId -> IO ()
-- debugRegion conn ind yr reg = do
--   rows <- query conn
--     "SELECT cr.country, o.value \
--     \FROM country_region cr \
--     \LEFT JOIN observation o \
--     \  ON o.country = cr.country \
--     \ AND o.indicator = ? \
--     \ AND o.year = ? \
--     \WHERE cr.region = ? \
--     \ORDER BY cr.country"
--     (ind, yr, reg)
--       :: IO [(CountryId, Maybe Value)]

--   mapM_ print rows

-- testdr = do   
--     conn <- open "test.sqlite"
--     debugRegion conn popid (Year 2024) (RegionId "USCAN")
--     debugRegion conn popid (Year 2024) (RegionId "EUROPE")
--     debugRegion conn popid (Year 2024) (RegionId "SAMERICA")
--     close conn

--   where 
--         popid = indicatorId population -- IndicatorId "SP.POP.TOTL"