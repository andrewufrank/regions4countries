module RegionSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model
import R4C.Region

tests :: TestTree
tests =
  testGroup "Region"
    [ testCase "G7 has seven members" $
        length (countriesInRegion (RegionId "G7")) @?= 7

    , testCase "Austria is in EU" $
        assertBool "AUT should be in EU" $
          CountryId "AUT" `elem` countriesInRegion (RegionId "EU")

    , testCase "Austria is not in G7" $
        assertBool "AUT should not be in G7" $
          CountryId "AUT" `notElem` countriesInRegion (RegionId "G7")
    ]