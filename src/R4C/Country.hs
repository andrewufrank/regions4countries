-----------------------------------------------------------------------------
--
-- Module      :  Country
--  get the country data from WB files 
-- construct the regions and checks 
-----------------------------------------------------------------------------

module R4C.Country where

import Database.SQLite.Simple
import R4C.Model
import R4C.Database.Instances
import R4C.Database
import Study.Config 
import Study.Dataset
import Data.List 

lookupCountries :: Connection -> IO [Country]
lookupCountries conn =
    query_ conn
        "SELECT country, name, region, incomeGroup, specialNotes \
        \FROM country \
        \ORDER BY name"

getCountries4db :: IO ()
getCountries4db = do
    conn <- open dbPath
    countries <- lookupCountries conn
    countryTable <- lookupTable conn (dsIndicator population) (Year 2024)
    close conn

    -- mapM_ print countries
    -- dumpCountries countries

    putStr "allWBcodes ="
    -- print . map (unCountryId . countryId) $ countries
    let countriesOnly = filter (not . ("" ==) . countryRegion) countries 
    putStr "countriesOnly = "
    -- print . map (unCountryId. countryId) $ countriesOnly

    let smallCountries = filter ((< 10**7). cvValue) countryTable :: CountryTable
    putStr "smallCountries = "   -- scheidet schweiz und oesterreich aus 
    print . map (unCountryId. cvCountry) $ smallCountries
    mapM_ print smallCountries 
    -- let smallCountriesSize = matchCountryTable smallCountries 

ppCountry :: Country -> String
ppCountry c =
    "Country "
        ++ show (countryId c)
        ++ " "
        ++ show (countryName c)
        ++ " "
        ++ show (countryRegion c)
        ++ " "
        ++ show (countryIncomeGroup c)
        -- ++ " "
        -- ++ show (countrySpecialNotes c)   -- mostly issues with currency conversions

-- dumpCountries :: Connection -> IO ()
dumpCountries :: [Country] -> IO ()
dumpCountries cs = do
    putStrLn "countries :: [Country]"
    putStrLn "countries ="
    putStrLn "  ["
    putStrLn $ intercalate "\n  , " (map ppCountry cs)
    putStrLn "  ]"

