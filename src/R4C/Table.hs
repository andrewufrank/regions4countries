-----------------------------------------------------------------------------
--
-- Module      :   R4C.Table
--
-- Produce Markdown tables
-----------------------------------------------------------------------------

module R4C.Table where

import Data.List (intercalate, sortOn)
import Data.Ord (Down(..))
import qualified Data.Text as T
import Numeric (showFFloat)
import qualified Data.Scientific as Sc

import R4C.Model

type Column a = [(RegionId, Maybe a)]

type DTable = Column Double

data MdColumn = MdColumn
    { colTitle    :: String
    , colScale    :: Double
    , colDecimals :: Int
    , colValues   :: DTable
    } 
    deriving (Eq, Ord, Show)

markdownTable :: [RegionId] -> [MdColumn] -> String
markdownTable regions cols =
    unlines (header : separator : map row regions)
  where
    header =
        "| Region | " ++ intercalate " | " (map colTitle cols) ++ " |"

    separator =
        "|:---|" ++ concat (replicate (length cols) "---:|")  -- the colon controls alignement

    row r =
        "| " ++ showRegion r ++ " | "
        ++ intercalate " | " (map (cell r) cols)
        ++ " |"

    cell r col =
        case lookup r (colValues col) of
            Nothing        -> ""
            Just Nothing   -> ""
            Just (Just x)  ->
                showFFloat
                    (Just (colDecimals col))
                    (x / colScale col)
                    ""

    showRegion (RegionId t) =
        T.unpack t

data SortOrder
    = Ascending
    | Descending

sortRegionsByColumn
    :: SortOrder
    -> DTable
    -> [RegionId]
sortRegionsByColumn order table =
    case order of
        Ascending ->
            map fst $ sortOn snd rows

        Descending ->
            map fst $ sortOn (Down . snd) rows
  where
    rows = table
    
valueToDouble :: Value -> Double
valueToDouble (Value v) =
    Sc.toRealFloat v

toDTable :: [(RegionId, Maybe Value)] -> DTable
toDTable = map convert
  where
    convert (r, mv) =
        (r, fmap valueToDouble mv)