-----------------------------------------------------------------------------
--
-- Module      :   Database.hs
-- store (bulk load) the observations from the read CSV file 
-----------------------------------------------------------------------------

module R4C.Database   
-- ( openDB
--   , closeDB
--   , createSchema
--   , insertObservation
--   , insertObservations
--   , insertIndicator
--   , observations
--   , observationsForCountry
--   , observationsForIndicator
--   , CountryTable
--   , CountryValue(..)
--   , lookupTable
--   ) 
  where

import Database.SQLite.Simple

import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import R4C.Model
  
openDB :: FilePath -> IO Connection
openDB = open

closeDB :: Connection -> IO ()
closeDB = close

createSchema :: Connection -> IO ()
createSchema conn =
    withTransaction conn $ do

        execute_ conn
            "CREATE TABLE IF NOT EXISTS country (\
            \ country TEXT PRIMARY KEY,\
            \ name TEXT NOT NULL,\
            \ region TEXT NOT NULL,\
            \ incomeGroup TEXT NOT NULL,\
            \ specialNotes TEXT NOT NULL\
            \)"

        execute_ conn
            "CREATE TABLE IF NOT EXISTS region (\
            \ region TEXT PRIMARY KEY,\
            \ name TEXT NOT NULL\
            \)"

        -- execute_ conn
        --     "CREATE TABLE IF NOT EXISTS country_region (\
        --     \ country TEXT NOT NULL,\
        --     \ region TEXT NOT NULL,\
        --     \ PRIMARY KEY(country, region)\
        --     \)"

        -- execute_ conn
        --     "CREATE TABLE IF NOT EXISTS indicator (\
        --     \ indicator TEXT PRIMARY KEY,\
        --     \ name TEXT NOT NULL\
        --     \)"
        execute_ conn
            "CREATE TABLE IF NOT EXISTS indicator (\
            \ indicator TEXT PRIMARY KEY,\
            \ name TEXT NOT NULL,\
            \ sourceNote TEXT NOT NULL,\
            \ sourceOrganization TEXT NOT NULL,\
            \ aggregation TEXT NOT NULL\
            \)"

        execute_ conn
            "CREATE TABLE IF NOT EXISTS observation (\
            \ country TEXT NOT NULL,\
            \ indicator TEXT NOT NULL,\
            \ year INTEGER NOT NULL,\
            \ value REAL NOT NULL,\
            \ PRIMARY KEY(country, indicator, year)\
            \)"

        -- indexes probably not needed - later?
        -- execute_ conn
        --     "CREATE TABLE IF NOT EXISTS observation (...)"

        -- execute_ conn
        --     "CREATE INDEX IF NOT EXISTS obs_indicator \
        --     \ON observation(indicator)"

        -- execute_ conn
        --     "CREATE INDEX IF NOT EXISTS obs_year \
        --     \ON observation(year)"

data CountryValue = CountryValue
    { cvCountry :: CountryId
    , cvValue   :: Double
    }

instance FromRow CountryValue where
    fromRow = CountryValue <$> field <*> field

type CountryTable = [CountryValue]

lookupTable
    :: Connection
    -> IndicatorId
    -> Year
    -> IO CountryTable
lookupTable conn ind yr =
    query conn
        "SELECT country, value \
        \FROM observation \
        \WHERE indicator = ? AND year = ?"
        (ind, yr)

------------------------------------------------------Country record 
insertCountry
    :: Connection
    -> Country
    -> IO ()

insertCountry conn country =
    execute conn
        "INSERT OR REPLACE INTO country \
        \VALUES (?,?,?,?,?)"
        ( countryId country
        , countryName country
        , countryRegion country
        , countryIncomeGroup country
        , countrySpecialNotes country
        )

insertCountries
    :: Connection
    -> [Country]
    -> IO ()

insertCountries conn =
    executeMany conn
        "INSERT OR REPLACE INTO country \
        \VALUES (?,?,?,?,?)"

countries4db
    :: Connection
    -> IO [Country]

countries4db conn =
    query_ conn
        "SELECT country, name, region, incomeGroup, specialNotes \
        \FROM country"

----------------------------------- indicators

insertIndicator
    :: Connection
    -> Indicator
    -> IO ()
insertIndicator conn ind =
    execute conn
        "INSERT OR REPLACE INTO indicator \
        \VALUES (?,?,?,?,?)"
        ( indicatorId ind
        , indicatorName ind
        , sourceNote ind
        , sourceOrganization ind
        , show (aggregation ind)  -- fills with sum
        )

-----------------------------------observations
insertObservation
    :: Connection
    -> Observation
    -> IO ()
insertObservation conn =
    execute conn
      "INSERT OR REPLACE INTO observation \
      \VALUES (?,?,?,?)"
      

insertObservations
    :: Connection
    -> [Observation]
    -> IO ()
insertObservations conn =
    executeMany conn
        "INSERT OR REPLACE INTO observation \
        \VALUES (?,?,?,?)"

-- insertIndicator
--     :: Connection
--     -> Indicator
--     -> IO ()
-- insertIndicator conn ind =
--     execute conn
--         "INSERT OR REPLACE INTO indicator VALUES (?,?)"
--         (indicatorId ind, indicatorName ind)

observations
    :: Connection
    -> IO [Observation]
observations conn =
    query_ conn
      "SELECT country,indicator,year,value \
      \FROM observation"

observationsForCountry
    :: Connection
    -> CountryId
    -> IO [Observation]
observationsForCountry conn c =
    query conn
      "SELECT country,indicator,year,value \
      \FROM observation \
      \WHERE country=?"
      (Only c)

observationsForIndicator
    :: Connection
    -> IndicatorId
    -> IO [Observation]
observationsForIndicator conn i =
    query conn
      "SELECT country,indicator,year,value \
      \FROM observation \
      \WHERE indicator=?"
      (Only i)
      