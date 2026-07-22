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
-- import R4C.Database 
-- import Database.SQLite.Simple
-- import Data.Maybe 
-- import R4C.Aggregate
    -- ( CountryPairs, valuesInRegion, matchCountryTables )
-- import Data.List  
import R4C.Territory 

--------------------------- statistics on list of [Doubles] or [(Double,Double)]
-- these list are constructed and are not empty

sum1 :: [(Double)] -> Double 
sum1 = sum 

average1 :: [(Double)] -> Double 
-- average [] = error ["average empty list"]
average1 xs = sum xs / (fromIntegral . length $ xs) 

wAverage1 :: [(Double, Double)] -> Double 
-- weighted average of non empty list; weight is second!
wAverage1 xsws = (sum . zipWith (*)  (map fst xsws) $ (map snd xsws)) / (sum . map snd $ xsws)


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
    :: (Eq t, Show t) => TerryTable t Double
    -> TerryTable t Double
    -> Maybe Double
regionCorrelation xs ys =
    pearson $
        terryValues $
            matchTerryTables xs ys



weightedMean
    :: TerryPairs t Double
    ->  Double
weightedMean pairs = wAverage1 . map terryTabel2pairs $ pairs 
--     | null pairs = Nothing
--     | sw == 0    = Nothing
--     | otherwise  = Just (sx / sw)
--   where
--     sw = sum [cvValue w | (_, w) <- pairs]

--     sx = sum
--             [ cvValue x * cvValue w
--             | (x, w) <- pairs
--             ]
