module Tab99spec (tests) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model 
import R4C.Export.Markdown
import BaseTest.Tab99
import  BaseTest.CountryExperiments 
import qualified Eins.Region as RBT 
-- import Test.HUnit.Diff (assertEqual)


tests :: TestTree
tests =
  testGroup
    "CountrySpec test from countryExperiments Tab99"
    [ testCase "testCase list of countries" $ do
        vals <- exp1a threeCountriesT 
        vals @?= 
            "| Region | Bev\246lkerung(MP) | GDP per capita(kPP$/P) | Flaeche(Mkm\178) |\n|:---|---:|---:|---:|\n| St. Martin (French part) | 0.026 |  | 0.000 |\n| Palau | 0.018 | 16 | 0.000 |\n| Naoero | 0.012 | 12 | 0.000 |\n| Tuvalu | 0.010 | 6 | 0.000 |\n"
         -- "| Region | Population(MP) | GDP per capita(kPP$/P) | Flaeche(Mkm\178) |\n|:---|---:|---:|---:|\n| St. Martin (French part) | 0.026 |  | 0.000 |\n| Palau | 0.018 | 16 | 0.000 |\n| Naoero | 0.012 | 12 | 0.000 |\n| Tuvalu | 0.010 | 6 | 0.000 |\n"

    , testCase "testCase regions 3a" $ do
        vals1 <- exp3a regionOrderTest threeEUcountriesT
        vals1 @?= "| Region | Bev\246lkerung(MP) | GNP(GUS$) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|---:|\n| USA & Kanada | 307.673 | 22145 | 13.394 | 70 |\n| S\252damerika | 107.603 | 987 | 4.389 | 20 |\n| Europa | 50.084 | 2168 | 0.416 | 52 |\n| Russland | 143.670 | 1724 | 17.125 | 39 |\n"
        -- "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | GNP(GUS$) |\n|:---|---:|---:|---:|---:|\n| USA & Kanada | 307.673 | 13.394 | 81 | 22145 |\n| S\252damerika | 107.603 | 4.389 | 24 | 987 |\n"
        --        "| Region | Population(MP) | GNP(GUS$) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|---:|\n| EUROPE |  |  |  |  |\n| Europ. Union | 450.228 | 17211 | 4.313 |  |\n| Gruppe 7 | 785.543 | 43949 | 27.354 |  |\n| Russland | 143.670 | 1724 | 17.125 |  |\n"

    , testCase "testCase regions 3b" $ do
        vals2 <- exp3b regionOrderTest threeEUcountriesT
        vals2 @?= 
            "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | GNP(GUS$) |\n|:---|---:|---:|---:|---:|\n| Finland | 5.620 | 0.338 | 57 | 293 |\n| Cyprus | 1.358 | 0.009 | 48 | 27 |\n| Portugal | 10.695 | 0.092 | 39 | 248 |\n"
         -- "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | GNP(GUS$) |\n|:---|---:|---:|---:|---:|\n| Finland | 5.620 | 0.338 | 63 | 293 |\n| Cyprus | 1.358 | 0.009 | 60 | 27 |\n| Portugal | 10.695 | 0.092 | 50 | 248 |\n"
         -- "| Region | Population(MP) | GNP(GUS$) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|---:|\n| Finland | 5.620 | 293 | 0.338 | 57 |\n| Cyprus | 1.358 | 27 | 0.009 | 48 |\n| Portugal | 10.695 | 248 | 0.092 | 39 |\n"

    , testCase "testCase regions and countries from exp7a md -- countries " $ do
        (md, _) <- exp7a threeEUcountriesT RBT.regionMembers
        md @?=  "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) |\n|:---|---:|---:|---:|\n| Finland | 5.620 | 0.338 | 65 |\n| Cyprus | 1.358 | 0.009 | 63 |\n| Portugal | 10.695 | 0.092 | 53 |\n"
        --      "| Region | Population(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | gnp(GPP$) |\n|:---|---:|---:|---:|---:|\n| Finland | 5.620 | 0.338 | 57 | 319 |\n| Cyprus | 1.358 | 0.009 | 48 | 65 |\n| Portugal | 10.695 | 0.092 | 39 | 413 |\n"

    , testCase "testCase regions and countries from exp7a md2 regions" $ do
        (_, md2) <- exp7a threeEUcountriesT RBT.regionMembers

        assertEqual "md2 from exp7a" md2  
            "| Region | Bev\246lkerung(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | gnp per cap.(k$/P) |\n|:---|---:|---:|---:|---:|\n| EUROPE |  |  |  |  |\n| Europ. Union | 45.604 | 0.344 | 67 | 0 |\n| Gruppe 7 | 194.292 | 12.499 | 75 | 0 |\n| Russland | 143.670 | 17.125 | 48 | 0 |\n"
        
        --  "| Region | Population(MP) | Flaeche(Mkm\178) | GDP per capita(kPP$/P) | gnp(GPP$) | gnp per cap.(k$/P) |\n|:---|---:|---:|---:|---:|---:|\n| EUROPE |  |  |  |  |  |\n| Europ. Union | 450.228 | 4.313 |  | 23453 | 52 |\n| Gruppe 7 | 785.543 | 27.354 |  | 47705 | 61 |\n| Russland | 143.670 | 17.125 |  | 5551 | 39 |\n"
    ]
    
    
threeCountriesT = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
threeEUcountriesT = (map CountryId ["FIN", "CYP", "PRT"]) 
regionOrderTest  = [
    RegionId "EUROPE",
    RegionId "EU",
    RegionId "G7",
    RegionId "RUSSIA"
    ]