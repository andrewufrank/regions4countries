module CountrySpec (tests) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model 
import R4C.Export.Markdown
import R4C.CountryExperiments

tests :: TestTree
tests =
  testGroup
    "test from countryExperiments"
    [ testCase "list of countries" $ do
        vals <- exp1a threeCountriesT 
        vals @?= 
            "| Region | Population(P) | GNP PP per Capita(TODO) | Flaeche(km\178) |\n|:---|---:|---:|---:|\n| St. Martin (French part) | 26129 |  | 50 |\n| Palau | 17695 | 21059 | 460 |\n| Naoero | 11947 | 13612 | 20 |\n| Tuvalu | 9646 | 6366 | 30 |\n"

    , testCase "regions" $ do
        vals12 <- exp3a regionOrderTest threeCountriesT
        vals12 @?= (
            "| Region | Population(P) | GNP PP per Capita(TODO) | Flaeche(km\178) |\n|:---|---:|---:|---:|\n| EUROPE |  |  |  |\n| Europ. Union | 450228188 | 1789375 | 4312962 |\n| Gruppe 7 | 785543447 | 467423 | 27353659 |\n| Russland | 143669648 | 47686 | 17125190 |\n"
            ,"| Region | Population(P) | GNP PP per Capita(TODO) | Flaeche(km\178) |\n|:---|---:|---:|---:|\n| St. Martin (French part) |  |  |  |\n| Palau |  |  |  |\n| Naoero |  |  |  |\n| Tuvalu |  |  |  |\n")

    ]
    
    
threeCountriesT = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
regionOrderTest  = [
    RegionId "EUROPE",
    RegionId "EU",
    RegionId "G7",
    RegionId "RUSSIA"
    ]