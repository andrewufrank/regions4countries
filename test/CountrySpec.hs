module CountrySpec (tests) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model 
import R4C.Export.Markdown
import BaseTest.Tab99

tests :: TestTree
tests =
  testGroup
    "CountrySpec test from countryExperiments"
    [ testCase "testCase list of countries" $ do
        vals <- exp1a threeCountriesT 
        vals @?= 
            "| Region | Population(P) | GDP per capita(kPP$/P) | Flaeche(km\178) |\n|:---|---:|---:|---:|\n| St. Martin (French part) | 26129 |  | 50 |\n| Palau | 17695 | 16 | 460 |\n| Naoero | 11947 | 12 | 20 |\n| Tuvalu | 9646 | 6 | 30 |\n"

    , testCase "testCase regions 3a" $ do
        vals1 <- exp3a regionOrderTest threeEUcountriesT
        vals1 @?= 
            "| Region | Population(P) | GNP(Giga) | Flaeche(km\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|---:|\n| EUROPE | 685752462 | 22804338148834 | 7161055 | 2090 |\n| Europ. Union | 450228188 | 17210556881304 | 4312962 | 1464 |\n| Gruppe 7 | 785543447 | 43949108388265 | 27353659 | 391 |\n| Russland | 143669648 | 1724054546331 | 17125190 | 39 |\n"

  , testCase "testCase regions 3b" $ do
        vals2 <- exp3b regionOrderTest threeEUcountriesT
        vals2 @?= 
            "| Region | Population(P) | GNP(Giga) | Flaeche(km\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|---:|\n| Finland | 5619911 | 292594560812 | 338480 | 57 |\n| Cyprus | 1358282 | 27255596951 | 9250 | 48 |\n| Portugal | 10694681 | 248482179228 | 92230 | 39 |\n"


    ]
    
    
threeCountriesT = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
threeEUcountriesT = (map CountryId ["FIN", "CYP", "PRT"]) 
regionOrderTest  = [
    RegionId "EUROPE",
    RegionId "EU",
    RegionId "G7",
    RegionId "RUSSIA"
    ]