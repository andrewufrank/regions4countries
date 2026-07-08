-----------------------------------------------------------------------------
--
-- Module      :   Query.hs
-- store (bulk load) the observations from the read CSV file 
-----------------------------------------------------------------------------

module R4C.Query where

import Database.SQLite.Simple
import R4C.Model

testq = do 
    conn <- open "test.sqlite"
    obs <- showObservations conn
    mapM_ print obs 

    rows <- surfaceAreaAustria conn
    mapM_ print rows

    avg <- averageValue conn (CountryId "AUT") (IndicatorId "AG.SRF.TOTL.K2")
    print avg 

    close conn

showObservations conn = do
    rows :: [Observation] <- query_ conn
        "SELECT country, indicator, year, value \
        \FROM observation \
        \LIMIT 10"
    pure rows

surfaceAreaAustria :: Connection -> IO [YearValue]
surfaceAreaAustria conn = do
    rows :: [YearValue] <- query conn
        "SELECT year, value \
        \FROM observation \
        \WHERE country = ? \
        \AND indicator = ? \
        \ORDER BY year"
        ("AUT", "AG.SRF.TOTL.K2") 
    pure rows 

    

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
