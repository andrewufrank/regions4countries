-----------------------------------------------------------------------------
--
-- Module      :   R4C.Table
--
-- Produce Markdown tables
-----------------------------------------------------------------------------

module R4C.Table where

import Data.List  
import Data.Ord (Down(..))
import qualified Data.Text as T
import Numeric (showFFloat)
import qualified Data.Scientific as Sc

import R4C.Model

type Column a = [(RegionId, Maybe a)]

-- type DTable = Column Double -- replace with regionTable

data MdColumn = MdColumn
    { colTitle    :: String
    , colScale    :: Double
    , colDecimals :: Int
    , colValues   :: RegionTable
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
        case lookupRegion r (colValues col) of
            Nothing -> ""

            Just rv ->
                case rvValue rv of
                    Nothing -> ""

                    Just x -> showFFloat
                            (Just (colDecimals col))
                            (x / colScale col)
                            ""

    showRegion (RegionId t) =
        T.unpack t

lookupRegion
    :: RegionId
    -> RegionTable
    -> Maybe RegionValue
lookupRegion r =
    find (\rv -> rvRegion rv == r)

data SortOrder
    = Ascending
    | Descending

sortRegionsByColumn
    :: SortOrder
    -> RegionTable
    -> [RegionId]
-- sorts (attention: Nothing is lowest!)
sortRegionsByColumn order table =
    case order of
        Ascending ->
            map rvRegion $
                sortOn rvValue table

        Descending ->
            map rvRegion $
                sortOn (Down . rvValue) table
    
valueToDouble :: Value -> Double
valueToDouble (Value v) =
    Sc.toRealFloat v

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