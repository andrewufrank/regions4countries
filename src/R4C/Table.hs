-----------------------------------------------------------------------------
--
-- Module      :   Table

-- produce Markdown tables 
-----------------------------------------------------------------------------

module R4C.Table where 
    
import Data.List (intercalate)
import qualified Data.Text as T
import Numeric (showFFloat)

import R4C.Model
import qualified Data.Scientific as Sc

type DTable = [(RegionId, Maybe Double)]

data MdColumn = MdColumn
    { colTitle    :: String
    , colScale    :: Double
    , colDecimals :: Int
    , colTable    :: DTable
    }

markdownTable :: [RegionId] -> [MdColumn] -> String
markdownTable regions cols =
    unlines (header : separator : map row regions)
  where
    header =
        "| Region | " ++ intercalate " | " (map colTitle cols) ++ " |"

    separator =
        "|---|" ++ concat (replicate (length cols) "---|")

    row r =
        "| " ++ showRegion r ++ " | "
        ++ intercalate " | " (map (cell r) cols)
        ++ " |"

    cell r col =
        case lookup r (colTable col) of
            Nothing        -> ""
            Just Nothing   -> ""
            Just (Just x)  -> showFFloat (Just (colDecimals col)) (x / colScale col) ""

    showRegion (RegionId t) =
        T.unpack t


valueToDouble :: Value -> Double
valueToDouble (Value v) =
    Sc.toRealFloat v

toDTable :: [(RegionId, Maybe Value)] -> DTable
toDTable = map convert
  where
    convert (r, mv) =
        (r, fmap valueToDouble mv)