module Tab98spec (tests) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model 
import R4C.Export.Markdown
import BaseTest.Tab98
import  BaseTest.CountryExperiments 
import qualified Eins.Region as RBT 
-- import Test.HUnit.Diff (assertEqual)


tests :: TestTree
tests =
  testGroup
    "CountrySpec test from countryExperiments Tab98"
    [ testCase "testCase list of countries" $ do
        vals <- exp5a regionOrderTest  
        vals @?= 
            "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GNP PP  (TPP$) | GDP per capita(kPP$/P) | GDP per capita extensive(TPP$) |\n|:---|---:|---:|---:|---:|---:|\n| EU |  |  |  |  |  |\n| Russland | 144 | 17 | 6 | 39 | 6 |\n"

    , testCase "testCase regions 3a" $ do
        vals1 <- exp6b  threeEUcountriesT
        vals1 @?= "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GNP PP  (TPP$) | GDP per capita(kPP$/P) | GDP per capita extensive(TPP$) |\n|:---|---:|---:|---:|---:|---:|\n| Finland | 6 | 0 | 0 | 57 | 0 |\n| Cyprus | 1 | 0 | 0 | 48 | 0 |\n| Portugal | 11 | 0 | 0 | 39 | 0 |\n"

    ]
    
    
threeCountriesT = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
threeEUcountriesT = (map CountryId ["FIN", "CYP", "PRT"]) 
regionOrderTest  = [
    -- RegionId "EUROPE",
    RegionId "EU",
    -- RegionId "G7",
    RegionId "RUSSIA"
    ]
