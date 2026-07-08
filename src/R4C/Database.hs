-----------------------------------------------------------------------------
--
-- Module      :   Database.hs
-- store (bulk load) the observations from the read CSV file 
-----------------------------------------------------------------------------

module R4C.Database   ( openDB
  , closeDB
  , createSchema
  , insertObservation
  , insertObservations
  , insertIndicator
  , observations
  , observationsForCountry
  , observationsForIndicator
  ) where

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
            \ name TEXT NOT NULL\
            \)"

        execute_ conn
            "CREATE TABLE IF NOT EXISTS region (\
            \ region TEXT PRIMARY KEY,\
            \ name TEXT NOT NULL\
            \)"

        execute_ conn
            "CREATE TABLE IF NOT EXISTS country_region (\
            \ country TEXT NOT NULL,\
            \ region TEXT NOT NULL,\
            \ PRIMARY KEY(country, region)\
            \)"

        execute_ conn
            "CREATE TABLE IF NOT EXISTS indicator (\
            \ indicator TEXT PRIMARY KEY,\
            \ name TEXT NOT NULL\
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

insertIndicator
    :: Connection
    -> Indicator
    -> IO ()
insertIndicator conn ind =
    execute conn
        "INSERT OR REPLACE INTO indicator VALUES (?,?)"
        (indicatorId ind, indicatorName ind)

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
      