-----------------------------------------------------------------------------
--
-- Module      :   Database.hs
-- store (bulk load) the observations from the read CSV file
-----------------------------------------------------------------------------

module R4C.Import.Database
where

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

import Database.SQLite.Simple

import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import R4C.Import.Instances
import R4C.Model

openDB :: FilePath -> IO Connection
openDB = open

closeDB :: Connection -> IO ()
closeDB = close

createSchema :: Connection -> IO ()
createSchema conn =
    withTransaction conn $ do
        execute_
            conn
            "CREATE TABLE IF NOT EXISTS country (\
            \ country TEXT PRIMARY KEY,\
            \ name TEXT NOT NULL,\
            \ region TEXT NOT NULL,\
            \ incomeGroup TEXT NOT NULL,\
            \ specialNotes TEXT NOT NULL\
            \)"

        execute_
            conn
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
        execute_
            conn
            "CREATE TABLE IF NOT EXISTS indicator (\
            \ source TEXT NOT NULL,\
            \ indicator TEXT NOT NULL,\
            \ name TEXT NOT NULL,\
            \ sourceNote TEXT NOT NULL,\
            \ sourceOrganization TEXT NOT NULL,\
            \ aggregation TEXT NOT NULL,\
            \ PRIMARY KEY(source, indicator)\
            \)"
        -- \ aggregation TEXT NOT NULL\

        execute_
            conn
            "CREATE TABLE IF NOT EXISTS observation (\
            \ country TEXT NOT NULL,\
            \ source TEXT NOT NULL,\
            \ indicator TEXT NOT NULL,\
            \ year INTEGER NOT NULL,\
            \ value REAL NOT NULL,\
            \ PRIMARY KEY(source, country, indicator, year)\
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

-- data CountryValue = CountryValue
--     { cvCountry :: CountryId
--     , cvValue   :: Double
--     }

-- instance FromRow CountryValue where
--     fromRow = CountryValue <$> field <*> field

-- type CountryTable = [CountryValue]

lookupTable ::
    Connection ->
    IndicatorRef ->
    Year ->
    IO CountryTable
-- lookupTable :: () => Connection -> IndicatorId -> Year -> IO [TerryTable CountryId v]
-- lookupTable :: (ToField IndicatorId, ToField Year, Show a1, Show Year, Show v) => Connection -> IndicatorId -> Year -> IO [TerryTable CountryId v]
lookupTable conn ref yr = do
    res <-
        query
            conn
            "SELECT country, value \
            \FROM observation \
            \WHERE source = ? AND indicator = ? AND year = ?"
            (refSource ref, refIndicator ref, yr)
    if length res == 0
        then putStrLn ("lookupTable empty for " ++ show ref ++ show yr)
        else putStrLn ("lookupTable for " ++ show ref)
    return res -- :: IO [TerryTable CountryId v]
    ------------------------------------------------------Country record

insertCountry ::
    Connection ->
    Country ->
    IO ()
insertCountry conn country =
    execute
        conn
        "INSERT OR REPLACE INTO country \
        \VALUES (?,?,?,?,?)"
        ( countryId country
        , countryName country
        , countryRegion country
        , countryIncomeGroup country
        , countrySpecialNotes country
        )

insertCountries ::
    Connection ->
    [Country] ->
    IO ()
insertCountries conn =
    executeMany
        conn
        "INSERT OR REPLACE INTO country \
        \VALUES (?,?,?,?,?)"

countries4db ::
    Connection ->
    IO [Country]
countries4db conn =
    query_
        conn
        "SELECT country, name, region, incomeGroup, specialNotes \
        \FROM country"

----------------------------------- indicators

insertIndicator ::
    Connection ->
    Indicator ->
    IO ()
insertIndicator conn ind =
    execute
        conn
        "INSERT OR REPLACE INTO indicator \
        \VALUES (?,?,?,?,?,?)"
        ( source ind
        , indicatorId ind
        , indicatorName ind
        , sourceNote ind
        , sourceOrganization ind
        , aggregation ind
        )

indicators4db ::
    Connection ->
    IO [Indicator]
indicators4db conn =
    query_
        conn
        "SELECT source, indicator, name, sourceNote, sourceOrganization, aggregation FROM indicator"

-- \       aggregation \
-----------------------------------observations
insertObservation ::
    Connection ->
    Observation ->
    IO ()
insertObservation conn =
    execute
        conn
        "INSERT OR REPLACE INTO observation \
        \VALUES (?,?,?,?,?)"

insertObservations ::
    Connection ->
    [Observation] ->
    IO ()
insertObservations conn =
    executeMany
        conn
        "INSERT OR REPLACE INTO observation \
        \VALUES (?,?,?,?,?)"

-- insertIndicator
--     :: Connection
--     -> Indicator
--     -> IO ()
-- insertIndicator conn ind =
--     execute conn
--         "INSERT OR REPLACE INTO indicator VALUES (?,?)"
--         (indicatorId ind, indicatorName ind)

observations ::
    Connection ->
    IO [Observation]
observations conn =
    query_
        conn
        "SELECT country,source,indicator,year,value \
        \FROM observation"

observationsForCountry ::
    Connection ->
    CountryId ->
    IO [Observation]
observationsForCountry conn c =
    query
        conn
        "SELECT country,source,indicator,year,value \
        \FROM observation \
        \WHERE country=?"
        (Only c)

observationsForIndicator ::
    Connection ->
    IndicatorRef ->
    IO [Observation]
observationsForIndicator conn ref =
    query
        conn
        "SELECT country,source,indicator,year,value \
        \FROM observation \
        \WHERE source=? AND indicator=?"
        (refSource ref, refIndicator ref)

-- test examples, move to where used

-- x1 = do
--     conn <- open dbPath
--     t <- lookupTable conn (IndicatorId "NY.GNP.MKTP.PP.KD") (Year 2023)
--     putIOwords [showT t]
-- x2 = do
--     conn <- open dbPath
--     t <- lookupTable conn (IndicatorId "NY.GDP.PCAP.PP.CD") (Year 2023)
--     putIOwords [showT t]
