-----------------------------------------------------------------------------
--
-- Module      :   Aggregate.hs
-- the function used for analysis 
-----------------------------------------------------------------------------

module R4C.Aggregate
      where

import Data.Maybe (mapMaybe)
import Database.SQLite.Simple  
import Data.List  
-- import qualified Data.Text as T

import R4C.Model 
import R4C.Database
import qualified Data.Map.Strict as Map 

-- showAggregate :: RegionMembers -> Connection -> Dataset -> Year -> RegionId -> IO ()
-- showAggregate memberships conn ds yr reg = do
--     result <- aggregate memberships conn ds yr reg
--     putStrLn $
--         show (dsName ds)
--         ++ " "
--         ++ show yr
--         ++ " "
--         ++ show reg
--         ++ " = "
--         ++ show result

valuesInRegion
    :: RegionMembers
    -> CountryTable
    -> RegionId
    -> CountryTable
valuesInRegion memberships table region =
    filter belongs table
  where
    countries =
        countriesInRegion memberships region

    belongs row =
        cvCountry row `elem` countries

aggregateRegion
    :: ([Double] -> Double)
    -> RegionMembers
    -> CountryTable
    -> RegionId
    -> Maybe Double
aggregateRegion agg memberships table region =
    case values of
        [] -> Nothing
        _ ->  Just (agg values)
  where
    values =  map cvValue $
            valuesInRegion memberships table region

-- sumAgg :: [Double] -> Double
-- sumAgg = sum

-- meanAgg :: [Double] -> Double
-- meanAgg xs = sum xs / fromIntegral (length xs)

aggregationFunction
    :: Aggregation
    -> [Double] -> Double
aggregationFunction Sum  xs = sum xs
aggregationFunction Mean xs = sum xs / fromIntegral (length xs)
aggregationFunction (WeightedBy _) _ =
    error "WeightedBy handled separately"

aggregate
    :: RegionMembers
    -> Connection
    -> Dataset
    -> Year
    -> IO RegionTable
-- | produce a table with for each region the value for the dataset 
aggregate memberships conn dataset year =
    case dsAggregation dataset of

        WeightedBy wt ->
            weightedAverage
                memberships
                conn
                (dsIndicator dataset)
                wt
                year

        agg -> do
            table <- lookupTable conn (dsIndicator dataset) year
            pure $
                aggregateTable
                    (aggregationFunction agg)
                    memberships
                    table

aggregateTable
    :: ([Double] -> Double)
    -> RegionMembers
    -> CountryTable
    -> RegionTable
aggregateTable f memberships table =
    [ RegionValue region
          (aggregateRegion f memberships table region)
    | (region, _) <- memberships
    ]

-- aggregationFunction
--     :: Dataset
--     -> [Double] -> Double
-- aggregationFunction ds xs =
--     case dsAggregation ds of
--         Sum -> sum xs
--         Mean -> sum xs / fromIntegral (length xs)

type CountryPairs = [(CountryValue, CountryValue)]

matchCountryTables
    :: CountryTable
    -> CountryTable
    -> CountryPairs
-- this is essentially a db join 
matchCountryTables xs ys =
    [ (x, y)
    | x <- xs
    , Just y <- [findCountry (cvCountry x) ys]
    ]

findCountry
    :: CountryId
    -> CountryTable
    -> Maybe CountryValue
findCountry c =
    find (\cv -> cvCountry cv == c)

weightedMean
    :: CountryPairs
    -> Maybe Double
weightedMean pairs
    | null pairs = Nothing
    | sw == 0    = Nothing
    | otherwise  = Just (sx / sw)
  where
    sw = sum [cvValue w | (_, w) <- pairs]

    sx = sum
            [ cvValue x * cvValue w
            | (x, w) <- pairs
            ]

weightedAverage
    :: RegionMembers
    -> Connection
    -> IndicatorId
    -> IndicatorId
    -> Year
    -> IO RegionTable
weightedAverage memberships conn valueInd weightInd year = do
    valueTable  <- lookupTable conn (  valueInd) year
    weightTable <- lookupTable conn (  weightInd) year

    pure
        [ RegionValue region
              (weightedMean
                  (matchCountryTables
                      (valuesInRegion memberships valueTable region)
                      (valuesInRegion memberships weightTable region)))
        | (region, _) <- memberships
        ]


combineRegionTables
    :: (Double -> Double -> Double)
    -> RegionTable
    -> RegionTable
    -> RegionTable
combineRegionTables f xs ys =
    [ RegionValue
        { rvRegion = r
        , rvValue  = lift2 f (rvValue x) (rvValue y)
        }
    | x <- xs
    , Just y <- [Map.lookup (rvRegion x) yMap]
    , let r = rvRegion x
    ]
  where
    yMap =
        Map.fromList
            [ (rvRegion y, y)
            | y <- ys
            ]

    lift2 g (Just a) (Just b) = Just (g a b)
    lift2 _ _ _               = Nothing

-- a better matchCountryTables (the join) wit Map 
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


---------------OLD 
-- lookupCountryValue :: CountryId -> CountryTable -> Maybe Double
-- -- get the value for the county (nothing if counry not existing)
-- lookupCountryValue c table =
--     case [ cvValue row | row <- table, cvCountry row == c ] of
--         []    -> Nothing
--         v : _ -> Just v


-- aggregate :: RegionMembers -> Connection -> Dataset -> Year -> [(RegionId, Maybe Double)])
-- -- calculate for a region the value from the country values
-- aggregate memberships conn ind year region = do
--     table <- lookupTable conn (dsIndicator ind) year

--     case aggregation ind of
--         Sum ->
--             pure  (sumTable memberships table region)

--         Mean ->
--             pure (meanTable memberships table region)

--         WeightedBy weightInd ->
--             weightedAverage memberships conn (dsIndicator ind) weightInd year region

-- -- sumTable :: CountryTable -> RegionId -> Maybe Double
-- sumTable :: RegionMembers -> CountryTable -> RegionId -> Maybe Double
-- sumTable memberships table region =
--     case valuesInRegion memberships table region of
--         [] -> Nothing
--         xs -> Just (sum . map snd $ xs)

-- -- meanTable :: CountryTable -> RegionId -> Maybe Double
-- meanTable :: RegionMembers -> CountryTable -> RegionId -> Maybe (CountryId, Double)
-- meanTable memberships table region =
--     case valuesInRegion memberships table region of
--         [] -> Nothing
--         xs -> Just (sum xs / fromIntegral (length xs))

-- weightedAverage :: RegionMembers -> Connection -> IndicatorId -> IndicatorId -> Year -> RegionId -> IO (Maybe Double)
-- weightedAverage memberships db valInd wtInd yr region = do
--     valTable <- lookupTable db valInd yr
--     wtTable  <- lookupTable db wtInd yr

--     let pairs =
--             [ (x, w)
--             | c <- countriesInRegion memberships region
--             , Just x <- [lookupCountryValue c valTable]
--             , Just w <- [lookupCountryValue c wtTable]
--             ]

--         sw = sum [w     | (_, w) <- pairs]
--         sx = sum [x * w | (x, w) <- pairs]

--     pure $
--         if sw == 0
--             then Nothing
--             else Just (sx / sw)

-- combineRegionTables
-- -- probably not needed anymore
--     :: (a -> b -> c)
--     -> [(RegionId, Maybe a)]
--     -> [(RegionId, Maybe b)]
--     -> [(RegionId, Maybe c)]
-- combineRegionTables f =
--     zipWith combine
--   where
--     combine (r1, mx) (r2, my)
--         | r1 /= r2 =
--             error $
--                 "combineRegionTables: region mismatch: "
--                 ++ show r1 ++ " /= " ++ show r2

--         | otherwise =
--             (r1, lift2 f mx my)

--     lift2 g (Just x) (Just y) = Just (g x y)
--     lift2 _ _ _               = Nothing