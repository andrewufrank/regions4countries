-----------------------------------------------------------------------------
--
-- Module      :   Aggregate.hs
-- the function used for analysis 
-----------------------------------------------------------------------------

module R4C.Aggregate
      where

import Data.Maybe (mapMaybe)
import Database.SQLite.Simple  
import Data.List (nub)
-- import qualified Data.Text as T

import R4C.Model 
import R4C.Database
-- import BaseTest.Region 
-- import BaseTest.Indicator

-- regionsList :: [RegionId]
-- regionsList = nub $ map fst regionMembers


-- showAggregate
--     :: Connection
--     -> Indicator
--     -> Year
--     -> RegionId
--     -> IO (Maybe Double)
showAggregate memberships conn ind yr reg = do
    result <- aggregate memberships conn ind yr reg
    putStrLn $
        show (indicatorName ind)
        ++ " "
        ++ show yr
        ++ " "
        ++ show reg
        ++ " = "
        ++ show result

valuesInRegion :: [(RegionId, CountryId)] -> CountryTable -> RegionId -> [Double]
valuesInRegion memberships table region =
    [ cvValue row
    | row <- table
    , cvCountry row `elem` countriesInRegion memberships region
    ]

lookupCountryValue :: CountryId -> CountryTable -> Maybe Double
lookupCountryValue c table =
    case [ cvValue row | row <- table, cvCountry row == c ] of
        []    -> Nothing
        v : _ -> Just v

-- aggregate
--     :: Connection
--     -> Indicator
--     -> Year
--     -> RegionId
--     -> IO (Maybe Double)
aggregate memberships conn ind year region = do
    table <- lookupTable conn (indicatorId ind) year

    case aggregation ind of
        Sum ->
            pure  (sumTable memberships table region)

        Mean ->
            pure (meanTable memberships table region)

        WeightedBy weightInd ->
            weightedAverage memberships conn (indicatorId ind) weightInd year region

-- sumTable :: CountryTable -> RegionId -> Maybe Double
sumTable memberships table region =
    case valuesInRegion memberships table region of
        [] -> Nothing
        xs -> Just (sum xs)

-- meanTable :: CountryTable -> RegionId -> Maybe Double
meanTable memberships table region =
    case valuesInRegion memberships table region of
        [] -> Nothing
        xs -> Just (sum xs / fromIntegral (length xs))

countriesInRegion
    :: [(RegionId, CountryId)]
    -> RegionId
    -> [CountryId]
countriesInRegion memberships rid =
    [ c | (r, c) <- memberships, r == rid ]

-- weightedAverage
--     :: Connection
--     -> IndicatorId
--     -> IndicatorId
--     -> Year
--     -> RegionId
--     -> IO (Maybe Double)
weightedAverage memberships db valInd wtInd yr region = do
    valTable <- lookupTable db valInd yr
    wtTable  <- lookupTable db wtInd yr

    let pairs =
            [ (x, w)
            | c <- countriesInRegion memberships region
            , Just x <- [lookupCountryValue c valTable]
            , Just w <- [lookupCountryValue c wtTable]
            ]

        sw = sum [w     | (_, w) <- pairs]
        sx = sum [x * w | (x, w) <- pairs]

    pure $
        if sw == 0
            then Nothing
            else Just (sx / sw)

combineRegionTables
    :: (a -> b -> c)
    -> [(RegionId, Maybe a)]
    -> [(RegionId, Maybe b)]
    -> [(RegionId, Maybe c)]
combineRegionTables f =
    zipWith combine
  where
    combine (r1, mx) (r2, my)
        | r1 /= r2 =
            error $
                "combineRegionTables: region mismatch: "
                ++ show r1 ++ " /= " ++ show r2

        | otherwise =
            (r1, lift2 f mx my)

    lift2 g (Just x) (Just y) = Just (g x y)
    lift2 _ _ _               = Nothing