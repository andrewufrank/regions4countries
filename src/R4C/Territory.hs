-----------------------------------------------------------------------------
--
-- Module      :   Territory.hs
-- the managing territory data (region or country 

-----------------------------------------------------------------------------
module R4C.Territory
    where 

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Correlation as C
import R4C.Model 
-- import R4C.Database 
-- import Database.SQLite.Simple
-- import Data.Maybe 
-- import R4C.Aggregate
    -- ( CountryPairs, valuesInRegion, matchCountryTables )
import Data.List  
import UniformBase


-- RegionValue is a a record {id, maybe value}, could be a map 
-- a version with map 
-- matchTerryTables
--     :: TerryTable t v 
--     -> TerryTable t v 
--     -> TerryPairs t v
-- matchTerryTables xs ys =
--     [ (x, y)
--     | x <- xs
--     , Just y <- [Map.lookup (tvCode x) yMap]
--     ]
--   where
--     yMap =
--         Map.fromList
--             [ (tvCode y, y)
--             | y <- ys
--             ]

matchTerryTables :: (Eq t, Show t) => [TerryValue t v1] -> TerryTable t v2 -> [(TerryValue t v1, TerryValue t v2)]
-- |find, not using map, bombs when not found 
-- this is essentially a db join 
matchTerryTables -- matchCountryTables
 xs ys =
    [ (x, y)
    | x <- xs
    , y <- [findTerry (tvCode x) ys]  -- fails if not found!
    ]

terryValues
-- must be checked that not one or the other value only 
-- was countryValues or regionValues
    :: TerryPairs t Double
    -> [(Double, Double)]
terryValues =
    map
        (\(x, y) ->
            (tvValue x, tvValue y))

terryTabel2pairs :: (TerryValue t Double, TerryValue t Double) -> (Double, Double)
terryTabel2pairs (c1,c2)= (tvValue c1, tvValue c2) 

findTerry -- findCountry
    :: (Eq t, Show t) => t
    -> TerryTable t v
    -> TerryValue t v
findTerry c  tt = fromJustNoteT ["findTerry code not found", showT c ]
                       . find (\cv -> tvCode cv == c) $ tt