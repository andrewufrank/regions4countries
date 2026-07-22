-----------------------------------------------------------------------------
--
-- Module      :   Statistics
-- the statistics used for analysis 
-- works on list of Doubles or pairs of doubles
-- data organisation is in territory
-----------------------------------------------------------------------------

module R4C.Statistics 
    where 

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Correlation as C
-- import R4C.Model 
-- import R4C.Database 
-- import Database.SQLite.Simple
-- import Data.Maybe 
-- import R4C.Aggregate
    -- ( CountryPairs, valuesInRegion, matchCountryTables )
-- import Data.List  
import R4C.Territory 

--------------------------- statistics on list of [Doubles] or [(Double,Double)]
--  returns nothing on empty list (or other reasons noe computable )

sum1 :: [(Double)] -> Maybe Double 
sum1 [ ]=  Nothing 
sum1 a = Just $ sum a

average1 :: [(Double)] -> Maybe Double 
-- average [] = error ["average empty list"]
average1 [] = Nothing 
average1 xs = Just $ sum xs / (fromIntegral . length $ xs) 

wAverage1 :: [(Double, Double)] -> Maybe Double 
-- weighted average of non empty list; weight is second!

wAverage1 [] = Nothing 
wAverage1 xsws = Just $ (sum . zipWith (*) 
         (map fst xsws) $ (map snd xsws)) / (sum . map snd $ xsws)


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


