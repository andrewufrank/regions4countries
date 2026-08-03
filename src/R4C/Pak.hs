-----------------------------------------------------------------------------
--
-- Module      :  Pak
--  datastructures of the type
--  data Pak t v = Pak { pDataSet :: Dataset, pTerryTable :: TerryTable t v }
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
    Pak t v ->
    Pak t v
scalePak f (Pak ds tab) = Pak ds (scaleTerryTable f tab)

sumPak3 ::
    Ord t =>
    [Pak t (WObs Double)] ->
    Pak t (WObs Double)
sumPak3 [] = error "sumPak3: cannot derive a dataset from an empty list"
sumPak3 paks@(Pak firstDataset _ : _) =
    Pak sumDataset summedTable
  where
    summedTable = sumTerryTables (map pTerryTable paks)
    datasets = map pDataSet paks
    shortNames = map dsShortName datasets
    indicators = map (unIndicatorId . dsIndicator) datasets

    sumDataset =
        firstDataset
            { dsIndicator = IndicatorId ("sum of " <> T.intercalate " + " indicators)
            , dsShortName = T.intercalate " + " shortNames
            , dsAggregation = Sum
            }

combinePak3 ::
    Ord t =>
    Operation ->
    Pak t (WObs Double) ->
    Pak t (WObs Double) ->
    Pak t (WObs Double)
-- | combines two pak with a function. 
-- combining weighted datasets works only for linear (specific affine) functions.
-- see document weightedAverage.md
combinePak3 operation left right =
    Pak combinedDataset combinedTable
  where
    combinedTable =
        combineTerryTables
            (operationFunction operation)
            (pTerryTable left)
            (pTerryTable right)
    leftDataset = pDataSet left
    rightDataset = pDataSet right
    symbol = s2t (operationSymbol operation)
    combineText getter = getter leftDataset <> " " <> symbol <> " " <> getter rightDataset
    isExtensive = case operation of
        Add -> extensive leftDataset && extensive rightDataset
        Subtract -> extensive leftDataset && extensive rightDataset
        Multiply -> extensive leftDataset /= extensive rightDataset
        Divide -> extensive leftDataset && not (extensive rightDataset)
    extensive dataset = dsAggregation dataset == Sum
    aggregation
        | isExtensive = Sum
        | dsAggregation leftDataset == dsAggregation rightDataset = dsAggregation leftDataset
        | otherwise = Mean
    unit = case operation of
        Add | dsUnit leftDataset == dsUnit rightDataset -> dsUnit leftDataset
        Subtract | dsUnit leftDataset == dsUnit rightDataset -> dsUnit leftDataset
        Divide | dsUnit leftDataset == dsUnit rightDataset -> ""
        _ -> dsUnit leftDataset <> symbol <> dsUnit rightDataset

    combinedDataset =
        leftDataset
            { dsIndicator =
                IndicatorId
                    ( unIndicatorId (dsIndicator leftDataset)
                        <> " " <> symbol <> " "
                        <> unIndicatorId (dsIndicator rightDataset)
                    )
            , dsShortName = combineText dsShortName
            , dsUnit = unit
            , dsAggregation = aggregation
            }

makePakExtensive ::
    Pak CountryId (WObs Double) ->
    Year ->
    Pak CountryId (WObs Double)

{- | Make a dataset extensive using its configured weight indicator.
The descriptor of the result is derived from the source dataset.
the year indicates which dataset be used for the weight dataset
-}
makePakExtensive pak1@(Pak ds1 _) yearDs =
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
makePakExtensive2 (Pak ds1 tab1) _year weightIndicatorId =
    Pak extensiveDs extensiveTable
  where
    extensiveTable = createTableExtensive tab1
    extensiveDs =
        ds1
            { dsIndicator =
                IndicatorId
                    ( "extensive of "
                        <> unIndicatorId (dsIndicator ds1)
                    )
            , dsShortName =
                dsShortName ds1 <> " extensive"
            , dsUnit = extensiveUnit weightIndicatorId (dsUnit ds1)
            , dsAggregation = Sum
            }

    extensiveUnit (IndicatorId "SP.POP.TOTL") unit =
        maybe unit id (T.stripSuffix "/P" unit)
    extensiveUnit (IndicatorId "AG.SRF.TOTL.K2") _ = "km\178"
    extensiveUnit indicator unit = unit <> " * " <> showT indicator


-- lookupRegionTable3 ::
--     Connection ->
--     [(RegionId, [CountryId])] ->
--     Dataset ->
--     Year ->
--     IO RegionTable3
-- -- fill for each region a countryTable with only its countries
-- lookupRegionTable3 conn regionDef ds yr = do
--     worldTab <- lookupTable conn (dsIndicator ds) yr
--     let regTab =
--             (ds, map (\(reg, cts) -> (reg, countryTable worldTab cts)) regionDef)
--     return regTab

-- lookupCountryTable3 :: Connection ->   Dataset -> Year -> IO CountryPak3
-- fill for each region a countryTable with only its countries
-- lookupCountryTable3 :: Connection -> Dataset -> Year -> IO (Dataset, [TerryValue CountryId ( Double)])

lookupCountryTable3 ::
    Connection ->
    Dataset ->
    Year ->
    IO CountryPak3
-- lookupCountryTable3 :: Connection -> Dataset -> Year -> IO (Dataset, [TerryValue CountryId v])
lookupCountryTable3 conn ds yr = do
    case dsAggregation ds of
        Sum -> do
            worldTab <- lookupTable conn (dsIndicator ds) yr
            let combTab = mkUnitWeight worldTab
                ctTab = Pak ds combTab
            return ctTab
        WeightedBy indicatorId -> do
            worldTab <- lookupTable conn (dsIndicator ds) yr
            weightTab <- lookupTable conn indicatorId yr -- issue TODO ??
            let combTab =
                    mkWeighted worldTab weightTab :: [TerryValue CountryId (WObs Double)]
                ctTab = Pak ds combTab
            return ctTab

-- combinesCountryTable3 :: (Ord t, Show t)
--     => Dataset -> (Double -> Double -> Double) -> (Dataset, TerryTable t Double) -> (Dataset, TerryTable t Double)
--     -> (Dataset, TerryTable t Double)
-- combinesCountryTable3 :: (Ord t, CombineVal v) => Dataset -> (CombineBase v -> CombineBase v -> CombineBase v) -> Pak t v -> Pak t v -> Pak t v
-- combinesCountryTable3 dsx f tab1 tab2 =
--     Pak dsx (combineTerryTables f (pTerryTable tab1) (pTerryTable tab2))

-- combining weighted datasets works only for linear (specific affine) functions.
-- see document weightedAverage.md

reg3CountryTable4 ::
    (Eq ct) =>
    [(rg, [ct])] ->
    [Pak ct v] ->
    [(Dataset, [(rg, TerryTable ct v)])]
-- construct region tables from country tables, does not aggregate values
reg3CountryTable4 regionMembers tabs =
    map (\(Pak ds tab) -> reg2 ds regionMembers tab) tabs
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
    map (\(dataset, table) -> Pak dataset table)
        . regtab3_regtab1
        . reg3CountryTable4 regionMembers

-- reg3CountryTable3 :: (Eq ct) => [(rg, [ct])] -> (Dataset, TerryTable ct ( v)) -> (Dataset, [(rg, TerryTable ct v)] )
-- reg3CountryTable3 regionMembers (ds, tab)  = reg2 ds regionMembers tab
