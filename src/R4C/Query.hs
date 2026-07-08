-----------------------------------------------------------------------------
--
-- Module      :   Query.hs
-- store (bulk load) the observations from the read CSV file 
-----------------------------------------------------------------------------

module R4C.Query where

import Database.SQLite.Simple
import R4C.Model

showObservations :: IO ()
showObservations = do
    conn <- open  "test.sqlite" -- "r4c.sqlite"

    rows :: [Observation] <- query_ conn
        "SELECT country, indicator, year, value \
        \FROM observation \
        \LIMIT 10"

    mapM_ print rows

    close conn

surfaceAreaAustria :: IO ()
surfaceAreaAustria = do
    conn <- open  "test.sqlite"
    -- rows :: [Observation] <- query conn
    --     "SELECT year, value \
    --     \FROM observation \
    --     \WHERE country = ? \
    --     \AND indicator = ? \
    --     \ORDER BY year"
    --     ("AUT", "AG.SRF.TOTL.K2")
    rows :: [YearValue] <- query conn
        "SELECT year, value \
        \FROM observation \
        \WHERE country = ? \
        \AND indicator = ? \
        \ORDER BY year"
        ("AUT", "AG.SRF.TOTL.K2") 

    mapM_ print rows

    close conn

averageValue
    :: Connection
    -> CountryId
    -> IndicatorId
    -> IO Double
averageValue conn country indicator = do

    [Only avg] <- query conn
        "SELECT AVG(value) \
        \FROM observation \
        \WHERE country = ? \
        \AND indicator = ?"
        (country, indicator)

    pure avg
