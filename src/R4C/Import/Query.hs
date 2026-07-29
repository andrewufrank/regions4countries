-----------------------------------------------------------------------------
--
-- Module      :   Query.hs
-- store (bulk load) the observations from the read CSV file 
-- R4C.WorldBank — import and parse data.
-- R4C.Database — create schema, insert rows, low-level database operations.
-- R4C.Query — retrieve observations in useful forms (by country, region, year, latest, time series).
-- R4C.Aggregate — compute sums, averages, weighted averages, etc., from queried data.
-----------------------------------------------------------------------------

module R4C.Import.Query where

import Database.SQLite.Simple
import R4C.Model
import R4C.Territory 
-- import R4C.Import.Instances
-- import R4C.Export.Table
import R4C.Import.Database 

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

allObservation
    :: Connection
    -> CountryId
    -> Year 
    -> IO (Maybe [Observation])
allObservation conn cid yr = do
    rows <- query conn
        "SELECT country, indicator, year, value \
        \FROM observation \
        \WHERE country = ? AND year = ?"
        (cid, yr)

    return $
        case rows of
            []    -> Nothing
            (x:xs) ->  Just (x:xs) 

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


lookupRegionTable3 :: Connection -> [(RegionId, [CountryId])] -> Dataset -> Year -> IO RegionTable3
-- fill for each region a countryTable with only its countries 
lookupRegionTable3 conn regionDef ds yr = do 
        worldTab <- lookupTable conn (dsIndicator ds) yr  
        let regTab = (ds, map (\(reg, cts) -> (reg, countryTable worldTab cts)) regionDef)
        return regTab 

lookupCountryTable3 :: Connection ->   Dataset -> Year -> IO CountryTable3
-- fill for each region a countryTable with only its countries 
lookupCountryTable3 conn  ds yr = do 
        worldTab <- lookupTable conn (dsIndicator ds) yr  
        let ctTab = (ds, worldTab) 
        return ctTab 
