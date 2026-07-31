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
-- import R4C.Territory 
import Data.List (sort)
import R4C.Model (WObs(..))
--------------------------- statistics on list of [Doubles] or [(Double,Double)]
--  returns nothing on empty list (or other reasons noe computable )

sum1 :: [(Double)] -> Maybe Double 
sum1 [ ]=  Nothing 
sum1 a = Just $ sum a

min1 :: [(Double)] -> Maybe Double 
min1 [ ] =  Nothing 
min1 a = Just $ minimum a

max1 :: [(Double)] -> Maybe Double 
max1 [ ] =  Nothing 
max1 a = Just $ maximum a

mean1 ::  [(Double)] -> Maybe Double  -- (Real a, Fractional b) => [a] -> Maybe b
mean1 [] = Nothing
mean1 xs = Just (realToFrac (sum xs) / fromIntegral (length xs))

median1 :: [Double] -> Maybe Double
median1 [] = Nothing
median1 xs =
    let ys = sort xs
        n  = length ys
        m  = n `div` 2
    in Just $
        if odd n
            then ys !! m
            else (ys !! (m - 1) + ys !! m) / 2

stdDev1 :: [Double] -> Maybe Double
stdDev1 [] = Nothing
stdDev1 xs =
    let n  = fromIntegral (length xs)
        mu = sum xs / n
        var = sum [ (x - mu)^2 | x <- xs ] / n
    in Just (sqrt var)

-- average1 :: [(Double)] -> Maybe Double 
-- -- average [] = error ["average empty list"]
-- average1 [] = Nothing 
-- average1 xs = Just $ sum xs / (fromIntegral . length $ xs) 

wAverage1 :: [(Double, Double)] -> Maybe Double 
-- weighted average of non empty list; weight is second!
wAverage1 [] = Nothing 
wAverage1 xsws = Just $ (sum . zipWith (*) 
         (map fst xsws) $ (map snd xsws)) / (sum . map snd $ xsws)

wAverage2 :: [WObs Double] -> Maybe Double 
-- weighted average of non empty list; weight is second!
wAverage2 [] = Nothing 
wAverage2 xsws = wAverage1 $ map  (\x -> (wobs x, wobs x * ww x)) xsws

-- Just $ (sum . zipWith (*) 
        --  (map wobs xsws) $ (map ww xsws)) / (sum . map ww $ xsws)

class Average a v where 
    avg :: [a ] -> Maybe v

instance Average Double Double where
    avg :: [Double] -> Maybe Double 
    avg = mean1
instance Average (WObs Double) Double where
    avg :: [WObs Double] -> Maybe Double 
    avg = wAverage2 

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


