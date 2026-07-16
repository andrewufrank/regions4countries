-----------------------------------------------------------------------------
--
-- Module      :   Query.hs
-- store (bulk load) the observations from the read CSV file 
-- R4C.WorldBank — import and parse data.
-- R4C.Database — create schema, insert rows, low-level database operations.
-- R4C.Query — retrieve observations in useful forms (by country, region, year, latest, time series).
-- R4C.Aggregate — compute sums, averages, weighted averages, etc., from queried data.
-----------------------------------------------------------------------------

module R4C.Query where

import Database.SQLite.Simple
import R4C.Model
import R4C.Database.Instances

latestObservation
    :: Connection
    -> CountryId
    -> IndicatorId
    -> IO (Maybe Observation)
latestObservation conn cid iid = do
    rows <- query conn
        "SELECT country, indicator, year, value \
        \FROM observation \
        \WHERE country = ? AND indicator = ? \
        \ORDER BY year DESC \
        \LIMIT 1"
        (cid, iid)

    return $
        case rows of
            []    -> Nothing
            (x:_) -> Just x

latestCountryValue
    :: Connection
    -> CountryId
    -> Indicator
    -> IO (Maybe YearValue)
latestCountryValue conn cid iid =
    fmap toYearValue <$> latestObservation conn cid (indicatorId iid)
  where
    toYearValue obs =
        YearValue
            (obsYear obs)
            (obsValue obs)

