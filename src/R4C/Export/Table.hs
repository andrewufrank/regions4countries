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

data MdColumn t v =   MdColumn
    { colTitle    :: String
    , colScale    :: Scale
    , colDecimals :: Int
    , colValues   :: TerryTable t v
    } 
    deriving (Eq, Ord, Show)

markdownTable ::  (Eq id, Show id, ShowCell id, ShowTerryId id) 
    => [TerryName id] -> [id] -> [MdColumn id Double] -> String
markdownTable names regions cols =
    unlines (header : separator : map row regions)
  where
    header =
        "| Region | " ++ intercalate " | " (map colTitle cols) ++ " |"

    separator =
        "|:---|" ++ concat (replicate (length cols) "---:|")  -- the colon controls alignement

    row r =
        "| " ++ showRegion names r ++ " | "
        ++ intercalate " | " (map (cell r) cols)
        ++ " |"

    cell :: (Eq id, Show id, ShowCell id) => id -> MdColumn id Double -> String 
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

    showRegion :: (Eq id, Show id, ShowTerryId id) =>  [TerryName id] -> id -> String
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
    -> RegionTable
    -> Maybe (RegionValue)
lookupRegion r =
    find (\rv -> tvCode rv == r)

lookupTerry 
    :: (Eq t) => t
    -> TerryTable t v
    -> Maybe (TerryValue t v)
lookupTerry r =
    find (\rv -> tvCode rv == r)


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

scaleRegionTable :: Double -> RegionTable -> RegionTable
scaleRegionTable k =
    map $ \rv ->
        rv { tvValue = fmap (* k) (tvValue rv) }


combineRegionTables
    :: (Double -> Double -> Double)
    -> RegionTable
    -> RegionTable
    -> RegionTable
combineRegionTables f xs ys =
    [ TerryValue
        { tvCode = r
        , tvValue  = lift2 f (tvValue x) (tvValue y)
        }
    | x <- xs
    , Just y <- [Map.lookup (tvCode x) yMap]
    , let r = tvCode x
    ]
  where
    yMap =
        Map.fromList
            [ (tvCode y, y)
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