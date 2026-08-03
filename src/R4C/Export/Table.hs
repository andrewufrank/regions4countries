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
import R4C.Territory (wrapMdCol1)
import qualified Data.Map.Strict as Map 
import UniformBase 
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import R4C.TerryTable
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
    => [TerryName id] -> [id] -> [Col id v] -> String
-- markdownTable :: [TerryName RegionId] -> [RegionId] -> [MdColumn RegionId Double] -> String
markdownTable names regions cols =
    unlines (header : separator : map row regions)
  where
    header =
        "| Region | " ++ intercalate " | " (map title_units cols) ++ " |"

    separator =
        "|:---------|" ++ concat (replicate (length cols) "---:|")  -- the colon controls alignement
                -- assume region names <= 10 char
    row r =
        "| " ++ showRegion names r ++ " | "
        ++ intercalate " | " (map (cell r) cols)
        ++ " |"

    -- cell :: (Eq id, Show id, ShowCell id) => id -> MdColumn id Double -> String 
    cell r col =
        case lookupTerry r (cTerrryTable col) of
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

-- | Render packages directly as a Markdown table.
markdownPakTable ::
    (Eq id, Show id, ShowTerryId id, ShowCell v, Magnitude v) =>
    [TerryName id] ->
    [id] ->
    [Pak id v] ->
    String
markdownPakTable names territories =
    markdownTable names territories . map wrapMdCol1


title_units :: Col t v -> [Char]
-- title_units col = colTitle md ++ "(" ++ show1scale (colScale md) ++ t2s (colUnit md) ++ ")"
--   where
--     md = cMd col
title_units col =
        colTitle md
            ++ "<br>*("
            ++ show1scale (colScale md)
            ++ t2s (colUnit md)
            ++ ")*"
      where
        md = cMd col
-------------
class ShowCell a where
    showCell :: Col i a -> a -> String

instance ShowCell Double where
    showCell :: Col i Double -> Double -> String
    showCell col x =
        showFFloat
            (Just (colDecimals (cMd col)))
            (x / scale2divisor (colScale (cMd col)))
            ""

instance ShowCell (WObs Double) where
    showCell :: Col i (WObs Double) -> (WObs Double) -> String
    showCell col (WObs x _) =
        showFFloat
            (Just (colDecimals (cMd col)))
            (x / scale2divisor (colScale (cMd col)))
            ""


instance ShowCell Text where
    showCell :: Col i Text -> Text -> String
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
