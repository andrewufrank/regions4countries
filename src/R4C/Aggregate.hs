-----------------------------------------------------------------------------
--
-- Module      :   Aggregate.hs
-- the function used for analysis 
-----------------------------------------------------------------------------

module R4C.Aggregate
      where

import Data.Maybe  
import Database.SQLite.Simple  
import Data.List
-- import qualified Data.Text as T

import R4C.Model 
import R4C.Import.Database
-- import qualified Data.Map.Strict as Map 
import R4C.Territory 
import R4C.Statistics
import R4C.Export.Table
import UniformBase 

aggregate
    :: RegionMembers
    -> Connection
    -> Dataset
    -> Year
    -> IO (MdColumn RegionId Double)
-- | produce a table with for each region the value for the dataset 
-- retrieves the dataset and possibly the weight 
aggregate memberships conn dataset year = do
    regTab <- case dsAggregation dataset of   -- could be dsExtensive

        WeightedBy wt ->                -- switch to use virtual (created) extensive dataset
                errorT ["WeightedBy must be handled with virtual dataset for ", showT dataset]
        --     weightedAverage
        --         memberships
        --         conn
        --         (dsIndicator dataset)
        --         wt
        --         year

        agg -> do
            table <- lookupTable conn (dsIndicator dataset) year
            pure $
                aggregateTable
                    (aggregationFunction agg)
                    memberships
                    table
    return (MdColumn {colTitle = t2s $ dsShortName dataset 
                    , colScale = Mega 
                    , colUnit = dsUnit dataset 
                    , colDecimals = dsDecimals dataset
                    , colValues = regTab})

aggregationFunction
    :: Aggregation
    -> [Double] -> Maybe Double
aggregationFunction Sum  xs = sum1 xs
aggregationFunction Mean xs = average1 xs 
aggregationFunction (WeightedBy _) _ =
    error "WeightedBy handled separately"


aggregateTable
    :: ([Double] -> Maybe Double)
    -> RegionMembers
    -> CountryTable
    -> (TerryTable RegionId Double)
aggregateTable f memberships table =
    [ TerryValue region
          (aggregateRegion f memberships table region)
    | (region, _) <- memberships
    ]


aggregateRegion
    :: ([Double] -> Maybe Double)
    -> RegionMembers
    -> CountryTable
    -> RegionId
    -> Maybe Double
aggregateRegion agg memberships table region =
    case values of
        [] -> Nothing
        _ ->  agg  . catMaybes $ values
  where
    values =  map tvValue $
            valuesInRegion memberships table region

-- weightedAverage
--     :: RegionMembers
--     -> Connection
--     -> IndicatorId
--     -> IndicatorId
--     -> Year
--     -> IO (TerryTable RegionId Double)
-- weightedAverage memberships conn valueInd weightInd year = do
--     valueTable  <- lookupTable conn (  valueInd) year
--     weightTable <- lookupTable conn (  weightInd) year

--     pure
--         [ TerryValue region
--               (weightedMean2
--                   (terryTables2pairs
--                       (valuesInRegion memberships valueTable region)
--                       (valuesInRegion memberships weightTable region)))
--         | (region, _) <- memberships
--         ]

regionCorrelation1
    :: (Eq t, Show t) => TerryPairs t Double
    -> Maybe Double
regionCorrelation1 pairs =
    pearson $
        terryValues $ pairs
   
regionCorrelation2 :: (Eq t, Show t) => MdColumn t Double -> MdColumn t Double -> Maybe Double
regionCorrelation2 tab1 tab2 = regionCorrelation1 (terryTables2pairs tab1 tab2)

weightedMean2
    :: TerryPairs t Double
    ->  Maybe Double
weightedMean2 [] = Nothing 
weightedMean2 pairs =   wAverage1 . terryValues $ pairs 
--     | null pairs = Nothing
--     | sw == 0    = Nothing
--     | otherwise  = Just (sx / sw)
--   where
--     sw = sum [cvValue w | (_, w) <- pairs]

--     sx = sum
--             [ cvValue x * cvValue w
--             | (x, w) <- pairs
--             ]





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

-- regionCorrelation
--     :: (Eq t, Show t) => TerryTable t Double
--     -> TerryTable t Double
--     -> Maybe Double
-- regionCorrelation xs ys =
--     pearson $
--         terryValues $
--             matchTerryTables xs ys

    -- lift2 g (Just a) (Just b) = Just (g a b)
    -- lift2 _ _ _               = Nothing


-- sumAgg :: [Double] -> Double
-- sumAgg = sum

-- meanAgg :: [Double] -> Double
-- meanAgg xs = sum xs / fromIntegral (length xs)






-- -- convenience wrapper to compute aggregate for a single region
-- aggregateSingleRegion
--     :: RegionMembers
--     -> Connection
--     -> Dataset
--     -> Year
--     -> RegionId
--     -> IO (Maybe Double)
-- aggregateSingleRegion memberships conn dataset year region = do
--     table <- aggregate memberships conn dataset year
--     pure $
--         case find (\rv -> tvCode rv == region) table of
--             Just rv -> tvValue rv
--             Nothing -> Nothing

-- aggregationFunction
--     :: Dataset
--     -> [Double] -> Double
-- aggregationFunction ds xs =
--     case dsAggregation ds of
--         Sum -> sum xs
--         Mean -> sum xs / fromIntegral (length xs)



-- matchCountryTables
--     :: CountryTable
--     -> CountryTable
--     -> CountryPairs
-- -- this is essentially a db join 
-- matchCountryTables xs ys =
--     [ (x, y)
--     | x <- xs
--     , Just y <- [findCountry (tvCode x) ys]
--     ]

-- findCountry
--     :: CountryId
--     -> CountryTable
--     -> Maybe CountryValue
-- findCountry c =
--     find (\cv -> tvCode cv == c)

-- weightedMean
--     :: CountryPairs
--     -> Maybe Double
-- weightedMean pairs
--     | null pairs = Nothing
--     | sw == 0    = Nothing
--     | otherwise  = lift2 (/) sx  sw
--   where
--     sw = sum [tvValue w | (_, w) <- pairs]

--     sx = sum
--             [ tvValue x * tvValue w
--             | (x, w) <- pairs
--             ]

--     lift2 g (Just a) (Just b) = Just (g a b)
--     lift2 _ _ _               = Nothing










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