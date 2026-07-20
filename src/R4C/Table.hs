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
import qualified Data.Map.Strict as Map 
import UniformBase 

type Column a = [(RegionId, Maybe a)]

-- type DTable = Column Double -- replace with regionTable
data Scale = Kilo | Mega | Giga | Tera | Centi | Unit | Milli| Micro | Nano | Pico deriving (Eq, Ord, Show )

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

data MdColumnX a =   MdColumn
    { colTitle    :: String
    , colScale    :: Scale
    , colDecimals :: Int
    , colValues   :: RegionTableX  a
    } 
    deriving (Eq, Ord, Show)

markdownTable :: ShowCell a => [Region] -> [RegionId] -> [MdColumnX a] -> String
markdownTable regionNames regions cols =
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

                    Just x -> showCell col x 
                        -- showFFloat
                        --     (Just (colDecimals col))
                        --     (x / scale2divisor (colScale  col))
                        --     ""

    showRegion :: RegionId -> String
    showRegion rid =
        case find (\r -> regionId r == rid) regionNames of
            Just r  -> T.unpack (regionName r)
            Nothing ->
                case rid of
                    RegionId t -> T.unpack t
                    
    -- showRegion (RegionId t) =
    --     T.unpack t

lookupRegion
    :: RegionId
    -> RegionTableX a
    -> Maybe (RegionValueX a)
lookupRegion r =
    find (\rv -> rvRegion rv == r)

-------------
class ShowCell a where
    showCell :: MdColumnX a -> a -> String

instance ShowCell Double where
    showCell :: MdColumnX Double -> Double -> String
    showCell col x =
        showFFloat
            (Just (colDecimals col))
            (x / scale2divisor (colScale col))
            ""

instance ShowCell Text where
    showCell :: MdColumnX Text -> Text -> String
    showCell _ = T.unpack


------------------
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

scaleRegionTable :: Double -> RegionTable -> RegionTable
scaleRegionTable k =
    map $ \rv ->
        rv { rvValue = fmap (* k) (rvValue rv) }


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