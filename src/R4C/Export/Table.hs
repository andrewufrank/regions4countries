{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :   R4C.Table
--
-- Produce Markdown tables
-----------------------------------------------------------------------------

module R4C.Export.Table where

import Data.List  
import Data.Ord (Down(..))
import qualified Data.Text as T
import Numeric (showFFloat)
import qualified Data.Scientific as Sc

import R4C.Model
import qualified Data.Map.Strict as Map 
import UniformBase 
import qualified Data.Map.Strict as Map
import Data.List (foldl')
-- import R4C.Pak

-- type Column a = [(RegionId, Maybe a)]

-- type DTable = Column Double -- replace with regionTable

scale2divisor ::   Scale -> Double
scale2divisor s = case s of  
                    Kilo -> 1000 
                    Mega-> 10**6
                    Giga-> 10**9 
                    Tera -> 10**12 
                    Centi -> 0.01
                    Unit -> 1 
                    Milli -> 10**(-3)
                    Micro -> 10**(-6)
                    Nano -> 10**(-9)
                    Pico -> 10**(-12)




markdownTable ::  (Eq id, Show id,   ShowTerryId id, ShowCell v) 
    => [TerryName id] -> [id] -> [MdColumn id v] -> String
-- markdownTable :: [TerryName RegionId] -> [RegionId] -> [MdColumn RegionId Double] -> String
markdownTable names regions cols =
    unlines (header : separator : map row regions)
  where
    header =
        "| Region | " ++ intercalate " | " (map title_units cols) ++ " |"

    separator =
        "|:---|" ++ concat (replicate (length cols) "---:|")  -- the colon controls alignement

    row r =
        "| " ++ showRegion names r ++ " | "
        ++ intercalate " | " (map (cell r) cols)
        ++ " |"

    -- cell :: (Eq id, Show id, ShowCell id) => id -> MdColumn id Double -> String 
    cell r col =
        case lookupTerry r (colValues col) of
            Nothing -> ""
            Just rv ->
                case tvValue rv of
                    Nothing -> ""
                    Just x -> showCell col x 
                        -- showFFloat
                        --     (Just (colDecimals col))
                        --     (x / scale2divisor (colScale  col))
                        --     ""

    -- showRegion :: (Eq id, Show id, ShowTerryId id) =>  [TerryName id] -> id -> String
    showRegion names rid =
        case find (\r -> terryId r == rid) names of
            Just r  -> T.unpack (terryName r)
            Nothing -> t2s $ showTerryId rid 
                -- case rid of
                --     Id t -> T.unpack t
                    
    -- showRegion (RegionId t) =
    --     T.unpack t

lookupRegion
    :: RegionId
    -> (TerryTable RegionId Double)
    -> Maybe (RegionValue)
lookupRegion r =
    find (\rv -> tvCode rv == r)

lookupTerry 
    :: (Eq t) => t
    -> TerryTable t v
    -> Maybe (TerryValue t v)
lookupTerry r =
    find (\rv -> tvCode rv == r)



title_units :: MdColumn t v -> [Char]
title_units col = colTitle col ++ "(" ++ (show1scale  . colScale $ col) ++ (t2s . colUnit $ col) ++ ")"
-------------
class ShowCell a where
    showCell :: MdColumn i a -> a -> String

instance ShowCell Double where
    showCell :: MdColumn i Double -> Double -> String
    showCell col x =
        showFFloat
            (Just (colDecimals col))
            (x / scale2divisor (colScale col))
            ""

instance ShowCell (WObs Double) where
    showCell :: MdColumn i (WObs Double) -> (WObs Double) -> String
    showCell col (WObs x _) =
        showFFloat
            (Just (colDecimals col))
            (x / scale2divisor (colScale col))
            ""


instance ShowCell Text where
    showCell :: MdColumn i Text -> Text -> String
    showCell _ = T.unpack


------------------
data SortOrder
    = Ascending
    | Descending

sortTerryByColumn
    :: (Ord v) => SortOrder
    -> TerryTable t v
    -> [t]
-- sorts (attention: Nothing is lowest!)
sortTerryByColumn order table =
    case order of
        Ascending ->
            map tvCode $
                sortOn tvValue table

        Descending ->
            map tvCode $
                sortOn (Down . tvValue) table
    
valueToDouble :: Value -> Double
valueToDouble (Value v) =
    Sc.toRealFloat v

-- scaleRegionTable :: Double -> (TerryTable RegionId Double) -> (TerryTable RegionId Double)
scaleRegionTable :: (Ord t, Show t, Eq t) => Double -> MdColumn t Double -> MdColumn t Double
scaleRegionTable k ct = ct{colValues = cv2}
    where   -- cv2 :: TerryTable RegionId Double 
            cv2 = map  (\rv -> rv { tvValue = fmap (* k) (tvValue rv) }) (colValues ct) 

-- | combine two MdColumns t v with a functioin 
combineMdTables :: (Ord t, Show t, Eq t) => Operation -> MdColumn t Double -> MdColumn t Double -> MdColumn t Double
combineMdTables f xs ys = MdColumn{colValues = xyt
        , colTitle = colTitle xs <> colTitle ys  --
        , colDecimals = min (colDecimals xs) (colDecimals ys)
        -- , colUnit = colUnit xs <> show f <>  colUnit ys
        , colUnit     = colUnit xs <> s2t (operationSymbol f) <> colUnit ys
        , colScale =  min (colScale xs)   (colScale ys)        }

    where 
        xt = colValues xs 
        yt = colValues ys 
        xyt = combineTerryTables (operationFunction f) xt yt 
        -- xytitle = colTitle xs <> colTitle ys  --
        -- xyDecimals = min (colDecimals xs) (colDecimals ys)
        -- xyUnit = colUnit xs <> " op " <> colUnit ys
        -- xyScale =  min (colScale xs)   (colScale ys)

-- combineTerryTables
--     :: Ord t
--     => (Double -> Double -> Double)
--     -> TerryTable t (WObs Double)
--     -> TerryTable t (WObs Double)
--     -> TerryTable t (WObs Double)
-- combineTerryTables f xs ys =
--     [ TerryValue
--         { tvCode  = tvCode x
--         , tvValue = combineMaybe f (tvValue x) (tvValue y)
--         }
--     | x <- xs
--     , Just y <- [Map.lookup (tvCode x) yMap]
--     ]
--   where
--     yMap =
--         Map.fromList
--             [ (tvCode y, y)
--             | y <- ys
--             ]

-- -------------


class CombineVal v where
    type CombineBase v

    combineMaybe
        :: (CombineBase v -> CombineBase v -> CombineBase v)
        -> Maybe v
        -> Maybe v
        -> Maybe v

instance CombineVal Double where
    type CombineBase Double = Double

    combineMaybe f (Just x) (Just y) =
        Just (f x y)

    combineMaybe _ _ _ =
        Nothing

instance (Eq v, Num v) => CombineVal (WObs v) where
    type CombineBase (WObs v) = v

    combineMaybe f
        (Just (WObs x wx))
        (Just (WObs y wy))
      | wx == wy =
            Just (WObs (f x y) wx)

      | wx == 1 =
            Just (WObs (f x y) wy)

      | wy == 1 =
            Just (WObs (f x y) wx)

      | otherwise =
            Nothing

    combineMaybe _ _ _ =
        Nothing


combineTerryTables
    :: (Ord t, CombineVal v)
    => (CombineBase v -> CombineBase v -> CombineBase v)
    -> TerryTable t v
    -> TerryTable t v
    -> TerryTable t v
combineTerryTables f xs ys =
    [ TerryValue
        { tvCode  = tvCode x
        , tvValue = combineMaybe f (tvValue x) (tvValue y)
        }
    | x <- xs
    , Just y <- [Map.lookup (tvCode x) yMap]
    ]
  where
    yMap =
        Map.fromList
            [ (tvCode y, y)
            | y <- ys
            ]

-- combineTerryTables
--     :: (Ord t, CombineVal v)
--     => (CombineBase v -> CombineBase v -> CombineBase v)
--     -> TerryTable t v
--     -> TerryTable t v
--     -> TerryTable t v
-- combineTerryTables f xs ys =
--     [ TerryValue
--         { tvCode  = tvCode x
--         , tvValue = combineMaybe f (tvValue x) (tvValue y)
--         }
--     | x <- xs
--     , Just y <- [Map.lookup (tvCode x) yMap]
--     ]
--   where
--     yMap =
--         Map.fromList
--             [ (tvCode y, y)
--             | y <- ys
--             ]

-- sumTerryTables
--     :: (Ord t, Eq t, Show t)
--     => [TerryTable t v]
--     -> TerryTable t v
sumTerryTables :: Ord t => [TerryTable t (WObs Double)] -> [TerryValue t (WObs Double)]
sumTerryTables []       = []
sumTerryTables [t]      = t
sumTerryTables (t:u:ts) =
    sumTerryTables (combineTerryTables (+) t u : ts)


data Operation = Add | Subtract | Multiply | Divide

operationFunction :: Operation -> Double -> Double -> Double
operationFunction Add      = (+)
operationFunction Subtract = (-)
operationFunction Multiply = (*)
operationFunction Divide   = (/)

operationSymbol :: Operation -> String
operationSymbol Add      = "+"
operationSymbol Subtract = "-"
operationSymbol Multiply = "*"
operationSymbol Divide   = "/"

-- toDTable :: [(RegionId, Maybe Value)] -> RegionTable
-- toDTable = map convert
--   where
--     convert (r, mv) =
--         (r, fmap valueToDouble mv)

-- better sort - nothing last 
-- Ascending ->
--     map rvRegion $
--         sortOn (sortKey . rvValue) table

-- Descending ->
--     map rvRegion $
--         sortOn (Down . sortKey . rvValue) table
-- sortKey
--     :: Maybe Double
--     -> (Bool, Double)
-- sortKey Nothing  = (True, 0)
-- sortKey (Just x) = (False, x)