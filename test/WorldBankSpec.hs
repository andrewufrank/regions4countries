module WorldBankSpec (tests) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Database.SQLite.Simple

import Test.Tasty
import Test.Tasty.HUnit

import R4C.Model
import R4C.Import.WorldBank 
import R4C.Import.Database
import BaseTest.Config

tests :: TestTree
tests =
  testGroup "WorldBankSpec"
    [ testCase "parseWorldBank reads multi-year CSV with Series headers" testParseWorldBank
    , testCase "import country metadata" testCountryMeta
    ]

sampleCSV :: BL.ByteString
sampleCSV = BL.pack $
  unlines
    [ "Some metadata line"
    , "Another metadata line"
    , "Country Name,Country Code,Series Name,Series Code,2020 [YR2020],2021 [YR2021]"
    , "Austria,AUT,\"Population, total\",SP.POP.TOTL,8916864,8955797"
    ]

testParseWorldBank :: Assertion
testParseWorldBank = do
    let (observations) = parseWBindicator sampleCSV

    -- indicatorId indicator @?= IndicatorId "SP.POP.TOTL"

    observations
        @?=
        [ Observation (CountryId "AUT") (IndicatorId "SP.POP.TOTL") (Year 2020) (Value 8916864)
        , Observation (CountryId "AUT") (IndicatorId "SP.POP.TOTL") (Year 2021) (Value 8955797)
        ]

testCountryMeta :: Assertion
testCountryMeta = do 
    conn <- open ":memory:"

    createSchema conn

    bytes <-
        BL.readFile
            "test/testdata/Metadata_Country_API_SM.POP.NETM_DS2_en_csv_v2_4998.csv"
    -- print (take 120 (BL.unpack bytes))
    let countries =
            parseWBcountries bytes

    insertCountries conn countries

    stored <- countries4db conn

    length stored @?= length countries

    assertBool
        "Austria missing"
        (CountryId "AUT" `elem` map countryId stored)

    closeDB conn


testIndicatorMeta :: Assertion
testIndicatorMeta = do

    conn <- open ":memory:"

    createSchema conn

    indicator <-
        readIndicatorMetadataFile
            "test/testdata/Metadata_Indicator_API_SM.POP.NETM_DS2_en_csv_v2_4998.csv"

    insertIndicator conn indicator

    stored <-
        indicators4db conn

    length stored @?= 1

    let actual =
            head stored

    indicatorId actual @?= indicatorId indicator

    indicatorName actual @?= indicatorName indicator

    sourceOrganization actual @?= sourceOrganization indicator

    assertBool
        "indicator is net migration"
        (indicatorName actual == "Net migration")
        
    closeDB conn


{-
---  old tests

test = do 
    -- bytes <- BL.readFile  "/home/frank/Desktop/buecher/nextOrder/WorldBankData/population/f27274b4-7384-4c6e-b81d-7ddf2ac9bb9a_Data.csv"
    bytes <- BL.readFile "/home/frank/Desktop/buecher/nextOrder/WorldBankData/surfaceArea/API_AG.SRF.TOTL.K2_DS2_en_csv_v2_4649.csv"
    let rows = decodeCSV bytes
    -- print (length rows)
    -- print (headerRow rows)   
    let hdr = headerRow rows 
    print (V.toList hdr)
    -- mapM_ print (zip [0 :: Int ..] (V.toList hdr))
    
    print (countryCodeColumn hdr)
    print (indicatorNameColumn hdr)
    print (indicatorCodeColumn hdr)
    print (yearColumns hdr)

    let row = head (countryRows rows)
    print (V.toList row)

    let ind = parseIndicator hdr row
    let ys  = yearColumns hdr
    let cc  = countryCodeColumn hdr

    print (parseCountry cc ys ind row)

dumpHeader :: Header -> IO ()
dumpHeader hdr =
    mapM_ print (zip [0 :: Int ..] (V.toList hdr))

    -}