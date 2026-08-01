-----------------------------------------------------------------------------
--
-- Module      :  Pak
--  datastructures of the type
--  type Pak t v = (Dataset, TerryTable t v)
-----------------------------------------------------------------------------

module R4C.Pak where

-- import Study.Config
-- import Study.Descriptor
-- import Data.List
-- import R4C.Import.Instances
import qualified Data.Map.Strict as M
import Database.SQLite.Simple
import R4C.Country
import R4C.Import.Database
import R4C.Model
import R4C.Territory
import R4C.TerryTable
import UniformBase

makePakExtensive ::
    Connection ->
    Pak CountryId (WObs Double) ->
    Dataset -> Year ->
    IO ((Pak CountryId (WObs Double)))
-- | make the dataset ds1 extensive with the weightq
-- from the weightIndicator 
-- and the same year 
-- it will have the descriptor based on newDescriptor
makePakExtensive conn pak1@(ds1, table) newDescriptor yearDs =
    case dsAggregation ds1 of
        Sum -> do
            putIOwords
                [ "makePakExtensive not required, "
                , dsShortName ds1
                , showT $ dsIndicator ds1
                , " is extensive"
                , "same pak returned - fix code!"
                ]
            pure pak1
        WeightedBy weightIndicator -> do
            putIOwords
                [ "makePakExtensive required, "
                , dsShortName ds1
                , showT $ dsIndicator ds1
                , " is weighted by "
                , showT weightIndicator
                ]

            makePakExtensive2
                conn
                pak1
                newDescriptor yearDs weightIndicator

makePakExtensive2 ::
    Connection ->
    Pak CountryId (WObs Double) ->
    Dataset -> Year -> 
    IndicatorId -> 
    IO ((Pak CountryId (WObs Double)))
makePakExtensive2 conn p1@(ds1, tab1) newDescriptor year weightIndicatorId = do
    putIOwords
        [ "makeTerryTableExtensive"
        , showT . dsIndicator $ ds1
        , "to"
        , showT weightIndicatorId
        ]
    let extensiveDs =
            newDescriptor
                { dsIndicator =
                    IndicatorId
                        ( "extensive of "
                            <> unIndicatorId (dsIndicator ds1)
                        )
                , dsShortName =
                    dsShortName ds1 <> " extensive"
                , dsDefinition =
                    "Extensive value calculated by multiplying "
                        <> dsShortName ds1
                        <> " by "
                        <> showT weightIndicatorId
                , dsUnit = dsUnit ds1 <> "ext"
                , dsAggregation = Sum
                , dsDecimals = 0
                , dsScale = Giga
                , dsExtensive = True
                , dsLastYear = Just year
                }
    -- let tab1double = dropUnitWeight tab1 
    newTab <- createTableExtensive conn tab1  weightIndicatorId year
    -- let newPak = case newPak of
    --         Nothing ->
    --             putIOwords
    --                 [ "makePakExtensive2"
    --                     "new createTableExtensive"
    --                 , showT . dsIndicator $ intensiveDs
    --                 ]
    --             return Nothing 
    --         Just n -> return $ Just (extensiveDs, n) -- TODO
    return (extensiveDs, newTab)

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
lookupCountryTable3 conn ds yr = do
    case dsAggregation ds of
        Sum -> do
            worldTab <- lookupTable conn (dsIndicator ds) yr
            let combTab = mkUnitWeight worldTab
                ctTab = (ds, combTab)
            return ctTab
        WeightedBy indicatorId -> do
            worldTab <- lookupTable conn (dsIndicator ds) yr
            weightTab <- lookupTable conn indicatorId yr -- issue TODO ??
            let combTab = mkWeighted worldTab weightTab :: [TerryValue CountryId (WObs Double)]
                ctTab = (ds, combTab)
            return ctTab

-- combinesCountryTable3 :: (Ord t, Show t)
--     => Dataset -> (Double -> Double -> Double) -> (Dataset, TerryTable t Double) -> (Dataset, TerryTable t Double)
--     -> (Dataset, TerryTable t Double)
combinesCountryTable3 dsx f tab1 tab2 = (dsx, combineTerryTables f (snd tab1) (snd tab2))

-- combining weighted datasets works only for linear (specific affine) functions.
-- see document weightedAverage.md

reg3CountryTable4 ::
    (Eq ct) =>
    [(rg, [ct])] ->
    [(Dataset, TerryTable ct (v))] ->
    [(Dataset, [(rg, TerryTable ct v)])]
-- construct region tables from country tables, does not aggregate values
reg3CountryTable4 regionMembers tabs =
    map (\(ds, tab) -> reg2 ds regionMembers tab) tabs
  where
    regTab1pop ::
        (Eq ct) =>
        TerryTable ct v ->
        (rg, [ct]) ->
        (rg, TerryTable ct v)
    -- make a single region  taboe
    regTab1pop tab (reg, cts) = (reg, countryTable tab cts)

    reg2 ::
        (Eq ct) =>
        Dataset ->
        [(rg, [ct])] ->
        TerryTable ct v ->
        (Dataset, [(rg, TerryTable ct v)])
    -- make all regions for a dataset  -> RegionTable3
    reg2 ds regionMembers tab = (ds, map (regTab1pop tab) regionMembers)

-- reg3CountryTable3 :: (Eq ct) => [(rg, [ct])] -> (Dataset, TerryTable ct ( v)) -> (Dataset, [(rg, TerryTable ct v)] )
-- reg3CountryTable3 regionMembers (ds, tab)  = reg2 ds regionMembers tab
