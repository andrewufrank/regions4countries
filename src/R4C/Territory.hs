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
import qualified Data.List.NonEmpty as NE 
import R4C.Statistics
import R4C.TerryTable (Magnitude, constructDataset)

valuesInRegion
    :: RegionMembers
    -> CountryTable
    -> RegionId
    -> CountryTable
valuesInRegion memberships table region =
    filter belongs table
  where
    countries =
        countriesInRegion memberships region

    belongs row =
        tvCode row `elem` countries

valuesInTable
    :: Eq t => [t]
    -> TerryTable t v
    -> TerryTable t v
-- | filter the code/value pairs for the countries 
valuesInTable countries table  =
    filter belongs table
  where
    belongs row = tvCode row `elem` countries
    
countryTable :: Eq t => TerryTable t v -> [t] -> TerryTable t v 
countryTable worldTab cts = valuesInTable cts worldTab 

aggregateTery
    :: ([Double] -> Maybe Double)
    -> [CountryId] -- what is to be included ? RegionMembers -- [(RegionId, [CountryId])]
    -> TerryTable CountryId ( Double)
    -- -> RegionId
    -> Maybe Double
-- | aggregation of a table lowest level
aggregateTery op memberships table  = op (catMaybes values)
  where
    values =  map tvValue $
            valuesInTable memberships table  


wrapMdCol1 :: Magnitude v => Pak t v -> Col t v
wrapMdCol1 (Pak dataset table) = constructDataset dataset table

-- convert RegionTable3 = (Dataset, [(RegionId, CountryTable)]) to (Dataset, RegionTable1)
-- regtab3_regtab1 (ds, tab) = (ds, sumCountryTables tab)
-- regtab3_regtab1 :: [(a, [(rt, TerryTable ct v)])] -> [(a, [TerryValue rt v])]
-- sum the countrytables to produce region lines 
-- canbe use for weighted averge
-- regtab3_regtab1 :: [(Dataset, [(t1, [TerryValue t2 v])])] -> [(Dataset, [TerryValue t1 v])]
regtab3_regtab1 :: [(a, [(t1, [TerryValue t2 (WObs Double)])])] -> [(a, [TerryValue t1 (WObs Double)])]
regtab3_regtab1 dstabs = map  oneTab3 dstabs

-- oneTab3 :: (Dataset, [(t1, [TerryValue t2 Double])]) -> (Dataset, [TerryValue t1 Double])
oneTab3 :: (a, [(t1, [TerryValue t2 (WObs Double)])]) -> (a, [TerryValue t1 (WObs Double)])
oneTab3 (ds, tab) = (ds, sumCountryTables2  tab) 
-- (\(ds,tab) -> (ds, val) dstabs
    -- where   isExtensive   = dsExtensive ds 
            -- val = if dsExtensive then sumCountryTables tab else tNothing 

aggregateTery2
    :: ([Double] -> Maybe Double)
    -- -> [CountryId] -- what is to be included ? RegionMembers -- [(RegionId, [CountryId])]
    -- include all 
    -> (RegionId, CountryTable)
    -- -> RegionId
    -> TerryValue RegionId  Double
-- | aggregation of a table lowest level
aggregateTery2 op table  = TerryValue 
    { tvCode = (fst table), tvValue = (op . catMaybes . map tvValue . snd $ table) }
--   where

aggregateTerry3    :: ([Double] -> Maybe Double)
    -> RegionTable3
    -- -> RegionId
    -> [RegionValue ] 
aggregateTerry3 op regionTab = map (aggregateTery2 op) (snd regionTab) 

-- sumCountryTables :: [(rt, TerryTable ct v)] -> [TerryValue rt v ] 
-- sum the values (must be extensional) in the country table 
-- and produce the sinle region value 
-- sumCountryTables2 :: Bool -> [(t1, [TerryValue t2 Double])] -> [TerryValue t1 Double]
sumCountryTables2 :: [(t1, [TerryValue t2 (WObs Double)])] -> [TerryValue t1 (WObs Double)]
sumCountryTables2  rct = map (oneRow ) rct 
    where

-- oneRow :: (rt, TerryTable ct v) -> TerryValue rt v
-- oneRow :: Bool -> (t1, [TerryValue t2 Double]) -> TerryValue t1 Double
oneRow :: (t1, [TerryValue t2 (WObs Double)]) -> TerryValue t1 (WObs Double)
oneRow (r, ct) = TerryValue {tvCode = r, tvValue = aggregate observations}
  where
    observations = catMaybes . map tvValue $ ct
    aggregate [] = Nothing
    aggregate xs = do
        value <- wAverage2 xs
        -- wAverage2 treats (wobs * ww) as the effective weight.  Retain an
        -- equivalent ww so aggregating the resulting regions gives the same
        -- value as aggregating their members directly.  Unit weights remain
        -- unit weights because table arithmetic uses 1 as its neutral marker.
        let weight
                | all ((== 1) . ww) xs = 1
                | value == 0 = 0
                | otherwise = sum [wobs x * ww x | x <- xs] / value
        pure (WObs value weight)
                


  
getOneRegionMany3 ::   [RegionTable3] -> RegionId -> [Col CountryId Double]
-- pack multiple regionTable from different datasets in MdColumn to convert to Md 
getOneRegionMany3 rct regid  = catMaybes $ map (\tab -> getOneRegion regid (fst tab)  (snd tab))  rct
  where

type RegionTable2 = [(RegionId, CountryTable)] -- nur hier gebraucht


getOneRegion :: RegionId -> Dataset -> [(RegionId, CountryTable)] -> Maybe (Col CountryId Double)
-- extract one country from a regionTable 
getOneRegion  regid dataset regtab = 
    case mbCountries of 
        Nothing -> Nothing -- putIOwords ["region", showT regid, "not found"]
        Just (_, ctTab) -> Just $ constructDataset dataset ctTab
    where
        mbCountries = find ((regid ==). fst) regtab  -- [(RegionId, CountryTable)]

-- wrapMdCol :: Dataset -> TerryTable t v -> Col t v
-- wrapMdCol dataset ct =
--     Col
--         { cMd =
--             MdCol
--                 { colTitle = t2s $ dsShortName dataset
--                 , colScale = dsScale dataset
--                 , colUnit = dsUnit dataset
--                 , colDecimals = dsDecimals dataset
--                 }
--         , cTerrryTable = ct
--         }

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

-- terryTables2pairs :: (Eq t, Show t) => [TerryValue t v1] -> [TerryValue t v2] -> [(TerryValue t v1, TerryValue t v2)]
-- |find, not using map, bombs when not found 
-- this is essentially a db join 
terryTables2pairs -- matchTerryTables -- matchCountryTables
 xsc ysc =
    [ (x, y)
    | x <- xs
    , y <- [findTerry (tvCode x) ys]  -- fails if not found!
    ]
  where
    xs = cTerrryTable xsc
    ys = cTerrryTable ysc

terryValues
-- must be checked that not one or the other value only 
-- was countryValues or regionValues
    :: TerryPairs t Double
    -> [(Double, Double)]
terryValues = catMaybes . map terryPairs2pairs
    -- map
    --     (\(x, y) ->
    --         (tvValue x, tvValue y))

terryPairs2pairs :: (TerryValue t Double, TerryValue t Double) -> Maybe (Double, Double)
terryPairs2pairs (c1,c2) = case (tvValue c1, tvValue c2)  of 
                    (Just v1, Just v2) -> Just (v1, v2)
                    _   -> Nothing

findTerry -- findCountry
    :: (Eq t, Show t) => t
    -> TerryTable t v
    -> TerryValue t v
findTerry c  tt = fromJustNoteT ["findTerry code not found", showT c ]
                       . find (\cv -> tvCode cv == c) $ tt
