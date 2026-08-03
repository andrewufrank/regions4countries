{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
--
-- Module      :   R4C.TerryTable
--
-- all operations on TerryTabe, which contain the values for a dataset
-----------------------------------------------------------------------------

module R4C.TerryTable where

import Data.List
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Scientific as Sc
import qualified Data.Text as T
import Numeric (showFFloat)
import R4C.Model
import UniformBase

class Magnitude v where
    magnitude :: v -> Double

instance Magnitude Double where
    magnitude = abs

instance Magnitude (WObs Double) where
    magnitude = abs . wobs

constructDataset ::
    (Magnitude v) => Dataset -> TerryTable t v -> Col t v
constructDataset dataset table =
    Col
        { cMd =
            MdCol
                { colTitle = t2s (dsShortName dataset)
                , colScale = scaleForMagnitude largestValue
                , colUnit = dsUnit dataset
                , colDecimals = dsDecimals dataset
                }
        , cTerrryTable = table
        }
  where
    largestValue =
        maximum (0 : [magnitude value | TerryValue _ (Just value) <- table])

    scaleForMagnitude value
        | value < 10 ** 5 = Unit
        | value < 10 ** 8 = Kilo
        | value < 10 ** 11 = Mega
        | value < 10 ** 14 = Giga
        | otherwise = Tera

createTableExtensive ::
    TerryTable CountryId (WObs Double) ->
    TerryTable CountryId (WObs Double)
createTableExtensive table =
    combineTerryTables (*) weightTabWeight table
  where
    weightTabWeight = mkUnitWeight (getValue4weights table)

lookupRegion ::
    RegionId ->
    (TerryTable RegionId Double) ->
    Maybe (RegionValue)
lookupRegion r =
    find (\rv -> tvCode rv == r)

lookupTerry ::
    (Eq t) =>
    t ->
    TerryTable t v ->
    Maybe (TerryValue t v)
lookupTerry r =
    find (\rv -> tvCode rv == r)

mkUnitWeight ::
    TerryTable CountryId Double ->
    TerryTable CountryId (WObs Double)
mkUnitWeight =
    map convert
  where
    convert (TerryValue c mv) =
        TerryValue c (fmap (\v -> WObs v 1) mv)

dropUnitWeight ::
    TerryTable CountryId (WObs Double) ->
    TerryTable CountryId Double
dropUnitWeight = map unconvert
  where
    unconvert (TerryValue c mwobs) =
        TerryValue c (fmap dropWeight mwobs)

    dropWeight (WObs v _) = v

getValue4weights ::
    TerryTable CountryId (WObs Double) ->
    TerryTable CountryId Double
getValue4weights = map recoverValues
  where
    recoverValues (TerryValue c mwobs) =
        TerryValue c (fmap getWeight mwobs)

    getWeight :: (WObs Double) -> Double
    getWeight ((WObs v w)) = w

-- table2double :: TerryTable CountryId (Wobs Double) - TerryTable CountryId Double
-- table2double [tvs] =

-- mkWeighted :: [TerryValue t a1] -> [TerryValue a2 v] -> [TerryValue t (WObs a1)]
mkWeighted ::
    (Ord k) =>
    [TerryValue k a] -> [TerryValue k a] -> [TerryValue k (WObs a)]

{- | add the weights to a table, using the value from the weightTable
used in the 'weighted by' case, so weightedAverage works
-}
mkWeighted valueTable weightTable =
    map addWeight valueTable
  where
    weights =
        M.fromList
            [ (country, weight)
            | TerryValue country weight <- weightTable
            ]

    addWeight (TerryValue country value) =
        TerryValue country $
            case M.lookup country weights of
                Nothing ->
                    Nothing
                Just weight ->
                    WObs <$> value <*> weight

valueToDouble :: Value -> Double
valueToDouble (Value v) =
    Sc.toRealFloat v

--
class ScaleByDouble v where
    scaleByDouble :: Double -> v -> v

instance ScaleByDouble Double where
    scaleByDouble k x = k * x

instance (ScaleByDouble v) => ScaleByDouble (WObs v) where
    scaleByDouble k (WObs x weight) =
        WObs
            (scaleByDouble k x)
            weight

scaleTerryTable ::
    (ScaleByDouble v) =>
    Double ->
    TerryTable t v ->
    TerryTable t v
scaleTerryTable k =
    map scaleValue
  where
    scaleValue (TerryValue c mv) =
        TerryValue c (fmap (scaleByDouble k) mv)

-- map (\rv -> rv {tvValue = fmap (* k) (tvValue rv)}) (colValues ct)

-- | combine two MdColumns t v with a functioin
combineMdTables ::
    ( Ord t
    , Show t
    , Eq t
    , CombineVal v
    , CombineBase v ~ Double
    ) =>
    Operation ->
    Col t v ->
    Col t v ->
    Col t v
combineMdTables f xs ys =
    Col
        { cMd =
            MdCol
                { colTitle = colTitle xmd <> colTitle ymd
                , colDecimals = min (colDecimals xmd) (colDecimals ymd)
                , colUnit = case f of
                    ToPercentOf -> "%"
                    FromPercentOf -> colUnit ymd
                    _ -> colUnit xmd <> s2t (operationSymbol f) <> colUnit ymd
                , colScale = case f of
                    ToPercentOf -> Unit
                    FromPercentOf -> colScale ymd
                    _ -> min (colScale xmd) (colScale ymd)
                }
        , cTerrryTable = xyt
        }
  where
    xmd = cMd xs
    ymd = cMd ys
    xt = cTerrryTable xs
    yt = cTerrryTable ys
    xyt = combineTerryTables (operationFunction f) xt yt

-- -------------

class CombineVal v where
    type CombineBase v

    combineMaybe ::
        (CombineBase v -> CombineBase v -> CombineBase v) ->
        Maybe v ->
        Maybe v ->
        Maybe v

instance CombineVal Double where
    type CombineBase Double = Double

    combineMaybe f (Just x) (Just y) =
        Just (f x y)
    combineMaybe _ _ _ =
        Nothing

instance (Eq v, Num v) => CombineVal (WObs v) where
    type CombineBase (WObs v) = v

    combineMaybe
        f
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

combineTerryTables ::
    (Ord t, CombineVal v) =>
    (CombineBase v -> CombineBase v -> CombineBase v) ->
    TerryTable t v ->
    TerryTable t v ->
    TerryTable t v
combineTerryTables f xs ys =
    [ TerryValue
        { tvCode = tvCode x
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

sumTerryTables ::
    (Ord t) =>
    [TerryTable t (WObs Double)] -> [TerryValue t (WObs Double)]
sumTerryTables [] = []
sumTerryTables [t] = t
sumTerryTables (t : u : ts) =
    sumTerryTables (combineTerryTables (+) t u : ts)

data Operation
    = Add
    | Subtract
    | Multiply
    | Divide
    | ToPercentOf
    | FromPercentOf

operationFunction :: Operation -> Double -> Double -> Double
operationFunction Add = (+)
operationFunction Subtract = (-)
operationFunction Multiply = (*)
operationFunction Divide = (/)
operationFunction ToPercentOf = \a b -> 100 * a / b
operationFunction FromPercentOf = \percentage base -> percentage * base / 100

operationSymbol :: Operation -> String
operationSymbol Add = "+"
operationSymbol Subtract = "-"
operationSymbol Multiply = "*"
operationSymbol Divide = "/"
operationSymbol ToPercentOf = "% of"
operationSymbol FromPercentOf = "% of"

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
