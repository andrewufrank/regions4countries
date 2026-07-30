module CountrySpec (tests) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model 
import R4C.Export.Markdown
import BaseTest.Tab99
import  BaseTest.CountryExperiments 
import qualified BaseTest.Region as RBT 

tests :: TestTree
tests =
  testGroup
    "CountrySpec test from countryExperiments"
    [ testCase "testCase list of countries" $ do
        vals <- exp1a threeCountriesT 
        vals @?= 
            "| Region | Population(MP) | GDP per capita(kPP$/P) | Flaeche(Mkm\178) |\n|:---|---:|---:|---:|\n| St. Martin (French part) | 0.026 |  | 0.000 |\n| Palau | 0.018 | 16 | 0.000 |\n| Naoero | 0.012 | 12 | 0.000 |\n| Tuvalu | 0.010 | 6 | 0.000 |\n"

    , testCase "testCase regions 3a" $ do
        vals1 <- exp3a regionOrderTest threeEUcountriesT
        vals1 @?= "| Region | Population(MP) | GNP(GUS$) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|---:|\n| EUROPE |  |  |  |  |\n| Europ. Union | 450.228 | 17211 | 4.313 |  |\n| Gruppe 7 | 785.543 | 43949 | 27.354 |  |\n| Russland | 143.670 | 1724 | 17.125 |  |\n"

    , testCase "testCase regions 3b" $ do
        vals2 <- exp3b regionOrderTest threeEUcountriesT
        vals2 @?= 
            "| Region | Population(MP) | GNP(GUS$) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|---:|\n| Finland | 5.620 | 293 | 0.338 | 57 |\n| Cyprus | 1.358 | 27 | 0.009 | 48 |\n| Portugal | 10.695 | 248 | 0.092 | 39 |\n"

    , testCase "testCase regions and countries from exp7a md -- countries " $ do
        (md, _) <- exp7a threeEUcountriesT RBT.regionMembers
        md @?=  "| Region | Population(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | gnp(GPP$) |\n|:---|---:|---:|---:|---:|\n| Finland | 5.620 | 0.338 | 57 | 319 |\n| Cyprus | 1.358 | 0.009 | 48 | 65 |\n| Portugal | 10.695 | 0.092 | 39 | 413 |\n"

    , testCase "testCase regions and countries from exp7a md2 regions" $ do
        (_, md2) <- exp7a threeEUcountriesT RBT.regionMembers

        md2 @?= "| Region | Population(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | gnp(GPP$) | gnp per cap.(k$/P) |\n|:---|---:|---:|---:|---:|---:|\n| EUROPE |  |  |  |  |  |\n| Europ. Union | 450.228 | 4.313 |  | 23453 | 52 |\n| Gruppe 7 | 785.543 | 27.354 |  | 47705 | 61 |\n| Russland | 143.670 | 17.125 |  | 5551 | 39 |\n"
    ]
    
    
threeCountriesT = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
threeEUcountriesT = (map CountryId ["FIN", "CYP", "PRT"]) 
regionOrderTest  = [
    RegionId "EUROPE",
    RegionId "EU",
    RegionId "G7",
    RegionId "RUSSIA"
    ]