-- | Minimal dataset registry for the study.
module Eins.Descriptor where

import Data.Text (Text)
import R4C.Model

sourcedDataset ::
    DataSource -> Text -> Text -> Text -> Aggregation -> Dataset
sourcedDataset source indicator shortName unit aggregation =
    Dataset
        { dsIndicator = IndicatorRef source (IndicatorId indicator)
        , dsShortName = shortName
        , dsUnit = unit
        , dsAggregation = aggregation
        , dsDecimals = if unit == "%" then 2 else 0
        }

dataset :: Text -> Text -> Text -> Aggregation -> Dataset
dataset = sourcedDataset WorldBank

derivedDataset :: Text -> Text -> Text -> Aggregation -> Dataset
derivedDataset = sourcedDataset Derived

energyInstituteDataset ::
    Text -> Text -> Text -> Aggregation -> Dataset
energyInstituteDataset = sourcedDataset EnergyInstitute

oilConsumption =
    energyInstituteDataset "oilcons_mt" "Ölverbrauch" "Mt" Sum
oilProduction =
    energyInstituteDataset "oilprod_mt" "Ölproduktion" "Mt" Sum
gasConsumption =
    energyInstituteDataset "gascons_bcm" "Gasverbrauch" "bcm" Sum
gasProduction =
    energyInstituteDataset "gasprod_bcm" "Gasproduktion" "bcm" Sum

population = dataset "SP.POP.TOTL" "Bevölkerung" "P" Sum
migrationNet = dataset "SM.POP.NETM" "Netto Migration" "P/y" Sum
surfaceArea = dataset "AG.SRF.TOTL.K2" "Flaeche" "km\178" Sum
arableLand = dataset "zzz" "Landwirtschaft" "km\178" Sum
gnp = dataset "NY.GNP.ATLS.CD" "GNP" "US$" Sum
gnpPP = dataset "NY.GNP.MKTP.PP.KD" "GNP PP  " "PP$" Sum
gdpPPpc =
    dataset
        "NY.GDP.PCAP.PP.CD"
        "GDP per capita"
        "PP$/P"
        populationWeighted
agriPercent = dataset "AG.LND.AGRI.ZS" "Landwirtschaft" "%" surfaceWeighted
forestPercent = dataset "AG.LND.FRST.ZS" "Wald Anteil" "%" surfaceWeighted
forest = dataset "AG.LND.FRST.K2" "Wald" "km\178" Sum
urban = dataset "AG.LND.TOTL.UR.K2" "Urban" "km\178" Sum
populationGrowthRate =
    dataset "SP.POP.GROW" "Wachstum Bevoelkerung" "%" populationWeighted
fertilityRate =
    ( dataset
        "SP.DYN.TFRT.IN"
        "Fertilitaetsrate"
        "P/woman"
        populationWeighted
    )
        { dsDecimals = 2
        }

-- Datasets used by Tab2.
cerealProduction = dataset "AG.PRD.CREL.MT" "Getreideproduktion" "t" Sum
arableLandPC =
    dataset
        "AG.LND.ARBL.HA.PC"
        "Ackerland"
        "ha/P"
        populationWeighted
ferilizerConsum = dataset "AG.CON.FERT.ZS" "Düngerverbrauch" "kg/ha" arableWeighted

-- Values calculated by the Tab2 study.
cerealFood = derivedDataset "cerealFood" "Menschliche Ernährung" "t" Sum
cerealDomesticUse =
    derivedDataset
        "cerealDomesticUse"
        "Gesamter Getreideverbrauch"
        "t"
        Sum
potentialCerealExport =
    derivedDataset
        "potentialCerealExport"
        "Potenzieller Getreideexport"
        "t"
        Sum
arableLandTotal = derivedDataset "arableLandTotal" "Ackerland" "ha" Sum
fertilizerConsumptionTotal =
    derivedDataset
        "fertilizerConsumptionTotal"
        "Düngerverbrauch gesamt"
        "kg"
        Sum

-- Virtual datasets. Their identifiers document that they are derived rather
-- than loaded directly from World Bank observations.
agriLand = derivedDataset "agriLand" "Landwirtschaft" "km\178" Sum
useableLand = derivedDataset "useableLand" "Nutzbares Land" "km\178" Sum
useableLandPerCent =
    derivedDataset
        "useableLandPercent"
        "Nutzbares Land"
        "%"
        surfaceWeighted
useableLandPC =
    derivedDataset
        "useableLandPerCapita"
        "Nutzbare Landflaeche"
        "a/P"
        populationWeighted
fertilityCount = derivedDataset "fertilityCount" "Kinder geboren" "P/y" Sum
netMigrationCount =
    derivedDataset
        "netMigrationRate"
        "MigrationRate netto"
        "P/Py"
        populationWeighted
popGrowthCount =
    derivedDataset
        "populationGrowthCount"
        "Wachstum Bevoelkerung"
        "P/y"
        Sum
gnpcc = derivedDataset "gnpPerCountry" "GNP2c" "$x" Sum

-- Placeholder descriptors retained for older experiments.
surfaxc1ePerCapita = cerealProduction
usableAxreaPerCapita1 =
    dataset
        "xxx2"
        "xxyy"
        "xxyy"
        (WeightedBy (IndicatorRef WorldBank (IndicatorId "xxyy")))
xxx6 =
    dataset
        "xxx3"
        "xxyy"
        "xxyy"
        (WeightedBy (IndicatorRef WorldBank (IndicatorId "xxyy")))
xxx5 =
    dataset
        "xxx4"
        "xxyy"
        "xxyy"
        (WeightedBy (IndicatorRef WorldBank (IndicatorId "xxyy")))
xxx4 =
    dataset
        "xxx5"
        "xxyy"
        "xxyy"
        (WeightedBy (IndicatorRef WorldBank (IndicatorId "xxx6")))
xxx33 =
    dataset
        "xxx7"
        "xxyy"
        "xxyy"
        (WeightedBy (IndicatorRef WorldBank (IndicatorId "xxx0")))
xxx2 =
    dataset
        "xxx8"
        "xxyy"
        "xxyy"
        (WeightedBy (IndicatorRef WorldBank (IndicatorId "xxx9")))

populationWeighted = WeightedBy (dsIndicator population)
surfaceWeighted = WeightedBy (dsIndicator surfaceArea)
arableWeighted = WeightedBy (IndicatorRef WorldBank (IndicatorId "AG.LND.ARBL.HA"))
