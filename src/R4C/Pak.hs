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
import R4C.Export.Table ( combineTerryTables ) 
import qualified Data.Map.Strict as M
import UniformBase 

makePakExtensive
    :: Connection
    -> Pak CountryId (WObs Double)
    -> Dataset 
    -> IO ( (Pak CountryId (WObs Double)))
makePakExtensive conn pak1@(ds, table) newDs =
    case dsAggregation ds of
        Sum -> do
            putIOwords
                [ "makePakExtensive not required, "
                , dsShortName ds
                , showT $ dsIndicator ds
                , " is extensive", "same pak returned - fix code!"
                ]
            pure pak1

        WeightedBy weightIndicator -> do
            putIOwords
                [ "makePakExtensive required, "
                , dsShortName ds
                , showT $ dsIndicator ds
                , " is weighted by "
                , showT weightIndicator
                ]

            makePakExtensive2
                conn
                pak1
                newDs

makePakExtensive2 :: Connection -> Pak CountryId (WObs Double) -> Dataset 
            -> IO ( (Pak CountryId (WObs Double)))
makePakExtensive2 conn p1@(weightDs, tab1) intensiveDs = do 
    putIOwords ["makeTerryTableExtensive", showT . dsIndicator $ ds1, 
                    "to", showT . dsIndicator $ intensiveDs]
    let extensiveDs =
            intensiveDs
                { dsIndicator =
                    IndicatorId
                        ( "extensive of "
                            <> unIndicatorId (dsIndicator intensiveDs)
                        )
                , dsShortName =
                    dsShortName intensiveDs <> " extensive"
                , dsDefinition =
                    "Extensive value calculated by multiplying "
                        <> dsShortName intensiveDs
                        <> " by "
                        <> dsShortName weightDs
                , dsUnit = dsUnit intensiveDs
                , dsAggregation = Sum
                , dsDecimals = 0
                , dsScale = Unit
                , dsExtensive = True
                , dsLastYear = dsLastYear intensiveDs
                }
    newTab <- createTableExtensive conn tab1 
    let newPak = case newPak of 
            Nothing -> errorT ["makePakExtensive2" 
                        "new createTableExtensive",
                         showT . dsIndicator $ intensiveDs] 
            Just n -> return  (extensiveDs, n)   -- TODO 
            
    return newPak 

createTableExtensive
    :: Connection
    -> TerryTable CountryId (WObs Double)
    -> Year 
    -> IO (TerryTable CountryId (WObs Double))
createTableExtensive conn table weightYear = do
    weightDs <- lookupCountryTable3 conn weightIndicator Year

    weightPak <- lookupCountryTable3 conn weightDs weightYear
    pure . snd $ weightDs




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

-- combinesCountryTable3 :: (Ord t, Show t) 
--     => Dataset -> (Double -> Double -> Double) -> (Dataset, TerryTable t Double) -> (Dataset, TerryTable t Double) 
--     -> (Dataset, TerryTable t Double)
combinesCountryTable3 dsx f tab1 tab2 = (dsx, combineTerryTables f (snd tab1) (snd tab2))

-- combining weighted datasets works only for linear (specific affine) functions. 
-- see document weightedAverage.md 


reg3CountryTable4 :: (Eq ct) => [(rg, [ct])] -> [(Dataset, TerryTable ct (v))] 
        -> [(Dataset, [(rg, TerryTable ct v)] )]
-- construct region tables from country tables, does not aggregate values 
reg3CountryTable4 regionMembers tabs   = 
    map (\(ds,tab) -> reg2 ds regionMembers tab) tabs
  where 
    regTab1pop :: (Eq ct) => TerryTable ct v  -> (rg, [ct]) 
        -> (rg, TerryTable ct v )
    -- make a single region  taboe 
    regTab1pop tab (reg, cts) =   (reg, countryTable tab cts) 

    reg2 :: (Eq ct) => Dataset ->    [(rg, [ct])] -> TerryTable ct v  
        ->   (Dataset, [(rg, TerryTable ct v)] )
    -- make all regions for a dataset  -> RegionTable3 
    reg2  ds regionMembers tab = (ds, map (regTab1pop tab) regionMembers)

-- reg3CountryTable3 :: (Eq ct) => [(rg, [ct])] -> (Dataset, TerryTable ct ( v)) -> (Dataset, [(rg, TerryTable ct v)] )
-- reg3CountryTable3 regionMembers (ds, tab)  = reg2 ds regionMembers tab 


    
