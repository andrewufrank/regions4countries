-----------------------------------------------------------------------------
--
-- Module      :  R4C.Pak
-- Description :  Dataset-aware construction and transformation of 'Pak's
--
-- A 'Pak' keeps a dataset descriptor together with its territory values.
-- The operations in this module update both parts together.
-----------------------------------------------------------------------------

module R4C.Pak (
    -- * Construction
    lookupCountryPak,

    -- * Transformations
    scalePak,
    setPakShortName,
    setPakUnitScale,
    makePakExtensive,

    -- * Combining packages
    sumPaks,
    combinePaks,

    -- * Territory conversion
    countryToRegionPaks,
) where

import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import R4C.Import.Database (lookupTable)
import R4C.Model
import R4C.Territory (countryTable, regtab3_regtab1)
import R4C.TerryTable
import UniformBase

{- | Load the country values for a dataset and attach the weights required by
its configured aggregation method.
-}
lookupCountryPak ::
    Connection ->
    Dataset ->
    Year ->
    IO CountryPak3
lookupCountryPak conn dataset year =
    case dsAggregation dataset of
        Sum -> do
            values <- lookupTable conn (dsIndicator dataset) year
            pure (Pak dataset (mkUnitWeight values))
        WeightedBy weightIndicator -> do
            values <- lookupTable conn (dsIndicator dataset) year
            weights <- lookupTable conn weightIndicator year
            pure (Pak dataset (mkWeighted values weights))

-- | Scale every value while retaining the dataset descriptor and weights.
scalePak ::
    (ScaleByDouble v) =>
    Double ->
    Pak t v ->
    Pak t v
scalePak factor (Pak dataset table) =
    Pak dataset (scaleTerryTable factor table)

-- | Replace the display name in a package's dataset descriptor.
setPakShortName :: T.Text -> Pak t v -> Pak t v
setPakShortName shortName pak =
    pak
        { pDataSet =
            (pDataSet pak)
                { dsShortName = shortName
                }
        }

{- | Replace the Unit and Scale in a package's dataset descriptor.
setPakShortName :: T.Text -> Pak t v -> Pak t v
-}
setPakUnitScale :: Text -> Pak t v -> Pak t v
setPakUnitScale un pak =
    pak
        { pDataSet =
            (pDataSet pak)
                { dsUnit = un
                }
        }

{- | Convert an intensive country package to an extensive one using the
weights already stored in its table. Packages configured with 'Sum' are
returned unchanged.

The year is retained in the API to identify the weight dataset used when
the package was constructed; the conversion itself does not perform IO.
-}
makePakExtensive ::
    Pak CountryId (WObs Double) ->
    Year ->
    Pak CountryId (WObs Double)
makePakExtensive pak@(Pak dataset _) _year =
    case dsAggregation dataset of
        Sum -> pak
        WeightedBy weightIndicator ->
            makePakExtensiveWith weightIndicator pak

makePakExtensiveWith ::
    IndicatorId ->
    Pak CountryId (WObs Double) ->
    Pak CountryId (WObs Double)
makePakExtensiveWith weightIndicator (Pak dataset table) =
    Pak extensiveDataset (createTableExtensive table)
  where
    extensiveDataset =
        dataset
            { dsIndicator =
                IndicatorId
                    ( "extensive of "
                        <> unIndicatorId (dsIndicator dataset)
                    )
            , dsShortName = dsShortName dataset <> " extensive"
            , dsUnit = extensiveUnit weightIndicator (dsUnit dataset)
            , dsAggregation = Sum
            }

    extensiveUnit (IndicatorId "SP.POP.TOTL") unit =
        maybe unit id (T.stripSuffix "/P" unit)
    extensiveUnit (IndicatorId "AG.SRF.TOTL.K2") _ = "km\178"
    extensiveUnit indicator unit = unit <> " * " <> showT indicator

{- | Sum packages and derive a matching descriptor for the result.

All packages must use compatible territory identifiers and weights. An
empty list is rejected because there is no descriptor to derive.
-}
sumPaks ::
    (Ord t) =>
    [Pak t (WObs Double)] ->
    Pak t (WObs Double)
sumPaks [] = error "sumPaks: cannot derive a dataset from an empty list"
sumPaks paks@(Pak firstDataset _ : _) =
    Pak sumDataset summedTable
  where
    summedTable = sumTerryTables (map pTerryTable paks)
    datasets = map pDataSet paks
    shortNames = map dsShortName datasets
    indicators = map (unIndicatorId . dsIndicator) datasets

    sumDataset =
        firstDataset
            { dsIndicator =
                IndicatorId ("sum of " <> T.intercalate " + " indicators)
            , dsShortName = T.intercalate " + " shortNames
            , dsAggregation = Sum
            }

{- | Combine two packages arithmetically and derive the descriptor of the
result. Weighted packages can only be combined when their weights are
compatible; see @docs/weightedAverage.md@.
-}
combinePaks ::
    (Ord t) =>
    Operation ->
    Pak t (WObs Double) ->
    Pak t (WObs Double) ->
    Pak t (WObs Double)
combinePaks operation left right =
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
        ToPercentOf -> False
        FromPercentOf -> extensive rightDataset
    extensive dataset = dsAggregation dataset == Sum
    aggregation
        | isPercentOf = Mean
        | isExtensive = Sum
        | dsAggregation leftDataset == dsAggregation rightDataset =
            dsAggregation leftDataset
        | otherwise = Mean
    isPercentOf = case operation of
        ToPercentOf -> True
        _ -> False
    unit = case operation of
        Add | dsUnit leftDataset == dsUnit rightDataset -> dsUnit leftDataset
        Subtract | dsUnit leftDataset == dsUnit rightDataset -> dsUnit leftDataset
        Divide | dsUnit leftDataset == dsUnit rightDataset -> ""
        ToPercentOf -> "%"
        FromPercentOf -> dsUnit rightDataset
        _ -> dsUnit leftDataset <> symbol <> dsUnit rightDataset

    combinedDataset =
        leftDataset
            { dsIndicator =
                IndicatorId
                    ( unIndicatorId (dsIndicator leftDataset)
                        <> " "
                        <> symbol
                        <> " "
                        <> unIndicatorId (dsIndicator rightDataset)
                    )
            , dsShortName = combineText dsShortName
            , dsUnit = unit
            , dsAggregation = aggregation
            }

{- | Aggregate country packages into region packages according to the supplied
region membership definition. The resulting packages retain weights so
that regions can be aggregated again.
-}
countryToRegionPaks ::
    (Eq country) =>
    [(region, [country])] ->
    [Pak country (WObs Double)] ->
    [Pak region (WObs Double)]
countryToRegionPaks regionMembers =
    map (\(dataset, table) -> Pak dataset table)
        . regtab3_regtab1
        . countryPaksByRegion regionMembers

countryPaksByRegion ::
    (Eq country) =>
    [(region, [country])] ->
    [Pak country value] ->
    [(Dataset, [(region, TerryTable country value)])]
countryPaksByRegion regionMembers =
    map splitPak
  where
    splitPak (Pak dataset table) =
        (dataset, map (restrictTable table) regionMembers)

    restrictTable table (region, countries) =
        (region, countryTable table countries)
