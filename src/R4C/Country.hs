-----------------------------------------------------------------------------
--
-- Module      :  Country
--  get the country data from WB files 
-- construct the regions and checks 
-----------------------------------------------------------------------------

module R4C.Country where

import Database.SQLite.Simple
import R4C.Model
import R4C.Import.Instances
import R4C.Import.Database
import Study.Config 
import Study.Descriptor
import Data.List 
import UniformBase

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
    countryTable :: CountryTable <- lookupTable conn (dsIndicator population) (Year 2024) 
    close conn

    -- mapM_ print countries
    dumpCountries countries

    -- putStr "allWBcodes ="
    -- print . map (unCountryId . countryId) $ countries
    let countriesOnly = filter (not . ("" ==) . countryRegion) countries :: [Country]
    putStr "countriesOnly = "
    print . map (unCountryId . countryId) $ countriesOnly

    let less10Mcountries = filter ((< 10**7). fromJustNote "smallCountries dwerwcc" . tvValue) countryTable :: CountryTable
    -- putStr "less10Mcountries = "   -- scheidet schweiz und oesterreich aus 
    -- putStr "less10Mcountries = "
    -- print . map (unCountryId .  tvCode) $ less10Mcountries

    let less1Mcountries = filter ((< 10**6). fromJustNote "smallCountries dwaaawcc" . tvValue) countryTable :: CountryTable
    -- putStr "less1Mcountries = "   --  
    -- putStr "less1Mcountries = "
    -- print . map (unCountryId . tvCode) $ less1Mcountries

    -- print . map (unCountryId. tvCode) $ smallCountries

    -- mapM_ print smallCountries 
    -- let smallCountriesSize = matchCountryTable smallCountries 
    return ()

ppCountry :: Country -> String
ppCountry c =
    "Terry ("
        ++ show (countryId c)
        ++ ") "
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

