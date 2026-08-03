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
import qualified Data.Text as T
import Database.SQLite.Simple
import R4C.Country
import R4C.Import.Database
import R4C.Model
import R4C.Territory
import R4C.TerryTable
import UniformBase


-- scalePak :: (Ord t, Show t) => (a, Double) -> MdColumn t v -> (a, MdColumn t v)
scalePak ::
    (Ord t, Show t, ScaleByDouble v) =>
    Double ->
    (Dataset, TerryTable t v) ->
    (Dataset, TerryTable t v)
scalePak f (ds, tab) = (ds, scaleTerryTable f tab)

makePakExtensive ::
    Pak CountryId (WObs Double) ->
    Year ->
    Pak CountryId (WObs Double)

{- | Make a dataset extensive using its configured weight indicator.
The descriptor of the result is derived from the source dataset.
-}
makePakExtensive pak1@(ds1, _) yearDs =
    case dsAggregation ds1 of
        Sum -> pak1
        WeightedBy weightIndicator ->
            makePakExtensive2
                pak1
                yearDs
                weightIndicator

makePakExtensive2 ::
    Pak CountryId (WObs Double) ->
    Year ->
    IndicatorId ->
    Pak CountryId (WObs Double)
makePakExtensive2 (ds1, tab1) year weightIndicatorId =
    (extensiveDs, createTableExtensive tab1)
  where
    extensiveDs =
        ds1
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
            , dsUnit = extensiveUnit weightIndicatorId (dsUnit ds1)
            , dsAggregation = Sum
            , dsDecimals = 0
            , dsScale = extensiveScale weightIndicatorId
            , dsExtensive = True
            , dsLastYear = Just year
            }

    extensiveUnit (IndicatorId "SP.POP.TOTL") unit =
        maybe unit id (T.stripSuffix "/P" unit)
    extensiveUnit (IndicatorId "AG.SRF.TOTL.K2") _ = "km\178"
    extensiveUnit indicator unit = unit <> " * " <> showT indicator

    extensiveScale (IndicatorId "SP.POP.TOTL") = Giga
    extensiveScale (IndicatorId "AG.SRF.TOTL.K2") = Kilo
    extensiveScale _ = Unit

lookupRegionTable3 ::
    Connection ->
    [(RegionId, [CountryId])] ->
    Dataset ->
    Year ->
    IO RegionTable3
-- fill for each region a countryTable with only its countries
lookupRegionTable3 conn regionDef ds yr = do
    worldTab <- lookupTable conn (dsIndicator ds) yr
    let regTab =
            (ds, map (\(reg, cts) -> (reg, countryTable worldTab cts)) regionDef)
    return regTab

-- lookupCountryTable3 :: Connection ->   Dataset -> Year -> IO CountryPak3
-- fill for each region a countryTable with only its countries
-- lookupCountryTable3 :: Connection -> Dataset -> Year -> IO (Dataset, [TerryValue CountryId ( Double)])

lookupCountryTable3 ::
    Connection ->
    Dataset ->
    Year ->
    IO (Dataset, [TerryValue CountryId (WObs Double)])
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
            let combTab =
                    mkWeighted worldTab weightTab :: [TerryValue CountryId (WObs Double)]
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

country2regionPak ::
    Eq ct =>
    [(rg, [ct])] ->
    [Pak ct (WObs Double)] ->
    [Pak rg (WObs Double)]
country2regionPak regionMembers =
    regtab3_regtab1 . reg3CountryTable4 regionMembers

-- reg3CountryTable3 :: (Eq ct) => [(rg, [ct])] -> (Dataset, TerryTable ct ( v)) -> (Dataset, [(rg, TerryTable ct v)] )
-- reg3CountryTable3 regionMembers (ds, tab)  = reg2 ds regionMembers tab
