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
            "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GNP PP  (GPP$) | GDP per capita(kPP$/P) | GDP per capita extensive(GPP$) |\n|:---|---:|---:|---:|---:|---:|\n| EU |  |  |  |  |  |\n| Russland | 143.670 | 17.125 | 5555 | 39 | 5593 |\n"

    , testCase "testCase regions 3a" $ do
        vals1 <- exp6b  threeEUcountriesT
        vals1 @?= "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GNP PP  (GPP$) | GDP per capita(kPP$/P) | GDP per capita extensive(GPP$) |\n|:---|---:|---:|---:|---:|---:|\n| Finland | 5.620 | 0.338 | 320 | 57 | 315 |\n| Cyprus | 1.358 | 0.009 | 40 | 48 | 63 |\n| Portugal | 10.695 | 0.092 | 397 | 39 | 401 |\n"

    ]
    
    
threeCountriesT = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
threeEUcountriesT = (map CountryId ["FIN", "CYP", "PRT"]) 
regionOrderTest  = [
    -- RegionId "EUROPE",
    RegionId "EU",
    -- RegionId "G7",
    RegionId "RUSSIA"
    ]
