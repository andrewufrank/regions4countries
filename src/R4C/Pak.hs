-----------------------------------------------------------------------------
--
-- Module      :  Pak
--  datastructures of the type 
--  type Pak t v = (Dataset, TerryTable t v)
-----------------------------------------------------------------------------

module R4C.Pak where

import Database.SQLite.Simple
import R4C.Model
-- import R4C.Import.Instances
import R4C.Import.Database
-- import Study.Config 
-- import Study.Descriptor
-- import Data.List 
import UniformBase
import R4C.Country
import R4C.Territory 
import R4C.Export.Table 


lookupRegionTable3 :: Connection -> [(RegionId, [CountryId])] -> Dataset -> Year -> IO RegionTable3
-- fill for each region a countryTable with only its countries 
lookupRegionTable3 conn regionDef ds yr = do 
        worldTab <- lookupTable conn (dsIndicator ds) yr  
        let regTab = (ds, map (\(reg, cts) -> (reg, countryTable worldTab cts)) regionDef)
        return regTab 

-- lookupCountryTable3 :: Connection ->   Dataset -> Year -> IO CountryTable3
-- fill for each region a countryTable with only its countries 
-- lookupCountryTable3 :: Connection -> Dataset -> Year -> IO (Dataset, [TerryValue CountryId ( Double)])

lookupCountryTable3 :: Connection -> Dataset -> Year -> IO (Dataset, [TerryValue CountryId (WObs Double)])
-- lookupCountryTable3 :: Connection -> Dataset -> Year -> IO (Dataset, [TerryValue CountryId v])
lookupCountryTable3 conn  ds yr = do 
    case dsAggregation ds of 
        Sum -> do   
            worldTab <- lookupTable conn (dsIndicator ds) yr  
            let combTab = mkUnitWeight worldTab 
                ctTab = (ds, combTab) 
            return ctTab 

        WeightedBy indicatorId -> do 
            worldTab <- lookupTable conn (dsIndicator ds) yr
            weightTab <- lookupTable conn indicatorId yr   -- issue TODO ??
            let combTab = mkWeighted worldTab weightTab :: [TerryValue CountryId (WObs Double)]
                ctTab = (ds, combTab)
            return ctTab 

mkUnitWeight :: TerryTable CountryId Double
             -> TerryTable CountryId (WObs Double)
mkUnitWeight =
    map convert
  where
    convert (TerryValue c mv) =
        TerryValue c (fmap (\v -> WObs v 1) mv)

mkWeighted
    :: TerryTable CountryId Double
    -> TerryTable CountryId Double
    -> TerryTable CountryId (WObs Double)
mkWeighted =
    zipWith combine
  where
    combine
        (TerryValue c1 mv)
        (TerryValue c2 mw)
      | c1 /= c2 =
            error $
                "mkWeighted: country mismatch: "
                ++ show c1 ++ " vs " ++ show c2
      | otherwise =
            TerryValue c1 (WObs <$> mv <*> mw)

combinesCountryTable3 :: (Ord t, Show t) 
    => Dataset -> (Double -> Double -> Double) -> (Dataset, TerryTable t Double) -> (Dataset, TerryTable t Double) 
    -> (Dataset, TerryTable t Double)
combinesCountryTable3 dsx f tab1 tab2 = (dsx, combineTerryTables f (snd tab1) (snd tab2))


reg3CountryTable4 :: (Eq ct) => [(rg, [ct])] -> [(Dataset, TerryTable ct (v))] -> [(Dataset, [(rg, TerryTable ct v)] )]
-- construct region tables from country tables 
reg3CountryTable4 regionMembers tabs   = map (\(ds,tab) -> reg2 ds regionMembers tab) tabs
  where 
    regTab1pop :: (Eq ct) => TerryTable ct v  -> (rg, [ct]) -> (rg, TerryTable ct v )
    -- make a single region  taboe 
    regTab1pop tab (reg, cts) =   (reg, countryTable tab cts) 

    reg2 :: (Eq ct) => Dataset ->    [(rg, [ct])] -> TerryTable ct v  ->   (Dataset, [(rg, TerryTable ct v)] )
    -- make all regions for a dataset  -> RegionTable3 
    reg2  ds regionMembers tab = (ds, map (regTab1pop tab) regionMembers)

-- reg3CountryTable3 :: (Eq ct) => [(rg, [ct])] -> (Dataset, TerryTable ct ( v)) -> (Dataset, [(rg, TerryTable ct v)] )
-- reg3CountryTable3 regionMembers (ds, tab)  = reg2 ds regionMembers tab 
