-----------------------------------------------------------------------------
--
-- Module      :   Aggregate.hs
-- the function used for analysis 
-----------------------------------------------------------------------------

module R4C.Aggregate
    ( aggregate
    , weightedAverage
    ) where

import Data.Maybe (mapMaybe)
import Database.SQLite.Simple  
import Data.List (nub)
-- import qualified Data.Text as T

import R4C.Model 
import R4C.Database
import R4C.Region 
import R4C.Indicator

regions2 :: [RegionId]
regions2 = nub $ map fst regionMembers

testPop = do 
    conn <- open "test.sqlite"
    pops <- mapM (showAggregate conn population (Year 2024)) regions2
    mapM_ print $ zip regions2 pops 
    close conn

testa = do 
    conn <- open "test.sqlite"
   
    -- showAggregate conn population (Year 2024) (RegionId "EU")
    -- showAggregate conn population (Year 2024) (RegionId "G7")
    showAggregate conn population (Year 2024) (RegionId "GULF")
    showAggregate conn population (Year 2024) (RegionId "RUSSIA")
    
    close conn    

showAggregate
    :: Connection
    -> Indicator
    -> Year
    -> RegionId
    -> IO (Maybe Double)
showAggregate conn ind yr reg = do
    result <- aggregate conn ind yr reg
    return result 
    -- putStrLn $
    --     show (indicatorName ind)
    --     ++ " "
    --     ++ show yr
    --     ++ " "
    --     ++ show reg
    --     ++ " = "
    --     ++ show result

valuesInRegion :: CountryTable -> RegionId -> [Double]
valuesInRegion table region =
    [ cvValue row
    | row <- table
    , cvCountry row `elem` countriesInRegion region
    ]

lookupCountryValue :: CountryId -> CountryTable -> Maybe Double
lookupCountryValue c table =
    case [ cvValue row | row <- table, cvCountry row == c ] of
        []    -> Nothing
        v : _ -> Just v

aggregate
    :: Connection
    -> Indicator
    -> Year
    -> RegionId
    -> IO (Maybe Double)
aggregate conn ind year region = do
    table <- lookupTable conn (indicatorId ind) year

    case aggregation ind of
        Sum ->
            pure  (sumTable table region)

        Mean ->
            pure (meanTable table region)

        WeightedBy weightInd ->
            weightedAverage conn (indicatorId ind) weightInd year region

sumTable :: CountryTable -> RegionId -> Maybe Double
sumTable table region =
    case valuesInRegion table region of
        [] -> Nothing
        xs -> Just (sum xs)

meanTable :: CountryTable -> RegionId -> Maybe Double
meanTable table region =
    case valuesInRegion table region of
        [] -> Nothing
        xs -> Just (sum xs / fromIntegral (length xs))

weightedAverage
    :: Connection
    -> IndicatorId
    -> IndicatorId
    -> Year
    -> RegionId
    -> IO (Maybe Double)
weightedAverage db valInd wtInd yr region = do
    valTable <- lookupTable db valInd yr
    wtTable  <- lookupTable db wtInd yr

    let pairs =
            [ (x, w)
            | c <- countriesInRegion region
            , Just x <- [lookupCountryValue c valTable]
            , Just w <- [lookupCountryValue c wtTable]
            ]

        sw = sum [w     | (_, w) <- pairs]
        sx = sum [x * w | (x, w) <- pairs]

    pure $
        if sw == 0
            then Nothing
            else Just (sx / sw)