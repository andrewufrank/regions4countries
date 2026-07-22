-----------------------------------------------------------------------------
--
-- Module      :   Statistics
-- the statistics used for analysis 
-----------------------------------------------------------------------------

module R4C.Statistics 
    where 

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Correlation as C
import R4C.Model 
import R4C.Import.Database 
import Database.SQLite.Simple
import Data.Maybe 
import R4C.Aggregate

-- type CountryPairs = [(CountryValue, CountryValue)]

type RegionPairs = [(RegionValue, RegionValue)]


matchRegionTables
    :: RegionTable
    -> RegionTable
    -> RegionPairs
matchRegionTables xs ys =
    [ (x, y)
    | x <- xs
    , Just y <- [Map.lookup (rvRegion x) yMap]
    ]
  where
    yMap =
        Map.fromList
            [ (rvRegion y, y)
            | y <- ys
            ]

-- matchCountryTables
--     :: CountryTable
--     -> CountryTable
--     -> CountryPairs
-- matchCountryTables xs ys =
--     [ (x, y)
--     | x <- xs
--     , Just y <- [Map.lookup (cvCountry x) yMap]
--     ]
--   where
--     yMap =
--         Map.fromList
--             [ (cvCountry y, y)
--             | y <- ys
--             ]

countryValues
    :: CountryPairs
    -> [(Double, Double)]
countryValues =
    map
        (\(x, y) ->
            (cvValue x, cvValue y))

regionValues
    :: RegionPairs
    -> [(Double, Double)]
regionValues =
    mapMaybe values
  where
    values (x, y) =
        case (rvValue x, rvValue y) of
            (Just a, Just b) ->
                Just (a, b)

            _ ->
                Nothing


toVectors
    :: [(Double, Double)]
    -> (V.Vector Double, V.Vector Double)
toVectors pairs =
    ( V.fromList [x | (x, _) <- pairs]
    , V.fromList [y | (_, y) <- pairs]
    )


pearson
    :: [(Double, Double)]
    -> Maybe Double
pearson pairs
    | length pairs < 2 =
        Nothing

    | otherwise =
         Just $
            C.pearson $
                V.fromList pairs
  where
    (xs, ys) =
        toVectors pairs

regionCorrelation
    :: RegionTable
    -> RegionTable
    -> Maybe Double
regionCorrelation xs ys =
    pearson $
        regionValues $
            matchRegionTables xs ys

countryCorrelation
    :: RegionMembers
    -> CountryTable
    -> CountryTable
    -> RegionId
    -> Maybe Double
countryCorrelation memberships xs ys region =
    pearson $
        countryValues $
            matchCountryTables
                (valuesInRegion memberships xs region)
                (valuesInRegion memberships ys region)


countryCorrelationIO
    :: RegionMembers
    -> Connection
    -> IndicatorId
    -> IndicatorId
    -> Year
    -> RegionId
    -> IO (Maybe Double)
countryCorrelationIO memberships conn ind1 ind2 year region = do
    t1 <- lookupTable conn ind1 year
    t2 <- lookupTable conn ind2 year

    pure $
        countryCorrelation
            memberships
            t1
            t2
            region
            