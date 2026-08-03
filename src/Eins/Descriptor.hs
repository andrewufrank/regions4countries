-- | Minimal dataset registry for the study.
module Eins.Descriptor where

import Data.Text (Text)
import R4C.Model

dataset :: Text -> Text -> Text -> Aggregation -> Dataset
dataset indicator shortName unit aggregation =
    Dataset
        { dsIndicator = IndicatorId indicator
        , dsShortName = shortName
        , dsUnit = unit
        , dsAggregation = aggregation
        }

population = dataset "SP.POP.TOTL" "Bevölkerung" "P" Sum
migrationNet = dataset "SM.POP.NETM" "Netto Migration" "P/y" Sum
surfaceArea = dataset "AG.SRF.TOTL.K2" "Flaeche" "km\178" Sum
arableLand = dataset "zzz" "Landwirtschaft" "km\178" Sum
gnp = dataset "NY.GNP.ATLS.CD" "GNP" "US$" Sum
gnpPP = dataset "NY.GNP.MKTP.PP.KD" "GNP PP  " "PP$" Sum
gdpPPpc = dataset "NY.GDP.PCAP.PP.CD" "GDP per capita" "PP$/P" populationWeighted
agriPercent = dataset "AG.LND.AGRI.ZS" "Landwirtschaft" "%" surfaceWeighted
forestPercent = dataset "AG.LND.FRST.ZS" "Wald Anteil" "%" surfaceWeighted
forest = dataset "AG.LND.FRST.K2" "Wald" "km\178" Sum
urban = dataset "AG.LND.TOTL.UR.K2" "Urban" "km\178" Sum
populationGrowthRate = dataset "SP.POP.GROW" "Wachstum Bevoelkerung" "%" populationWeighted
fertilityRate = dataset "SP.DYN.TFRT.IN" "Fertilitaetsrate" "P/woman" populationWeighted

-- Datasets used by Tab2.
cerealProduction = dataset "AG.PRD.CREL.MT" "Getreideproduktion" "t" Sum
arableLandPC = dataset "AG.LND.ARBL.HA.PC" "Ackerland pro Person" "ha/P" populationWeighted
ferilizerConsum = dataset "AG.CON.FERT.ZS" "Düngerverbrauch" "kg/ha" arableWeighted

-- Values calculated by the Tab2 study.
cerealFood = dataset "derived.cerealFood" "Menschliche Ernährung" "t" Sum
cerealDomesticUse = dataset "derived.cerealDomesticUse" "Gesamter Getreideverbrauch" "t" Sum
potentialCerealExport = dataset "derived.potentialCerealExport" "Potenzieller Getreideexport" "t" Sum
arableLandTotal = dataset "derived.arableLandTotal" "Ackerland" "ha" Sum
fertilizerConsumptionTotal = dataset "derived.fertilizerConsumptionTotal" "Düngerverbrauch gesamt" "kg" Sum

-- Virtual datasets. Their identifiers document that they are derived rather
-- than loaded directly from World Bank observations.
agriLand = dataset "derived.agriLand" "Landwirtschaft" "km\178" Sum
useableLand = dataset "derived.useableLand" "Nutzbares Land" "km\178" Sum
useableLandPerCent = dataset "derived.useableLandPercent" "Nutzbares Land" "%" surfaceWeighted
useableLandPC = dataset "derived.useableLandPerCapita" "Nutzbare Landflaeche" "a/P" populationWeighted
fertilityCount = dataset "derived.fertilityCount" "Kinder geboren" "P/y" Sum
netMigrationCount = dataset "derived.netMigrationRate" "MigrationRate netto" "P/Py" populationWeighted
popGrowthCount = dataset "derived.populationGrowthCount" "Wachstum Bevoelkerung" "P/y" Sum
gnpcc = dataset "derived.gnpPerCountry" "GNP2c" "$x" Sum

-- Placeholder descriptors retained for older experiments.
surfaxc1ePerCapita = cerealProduction
usableAxreaPerCapita1 = dataset "xxx2" "xxyy" "xxyy" (WeightedBy (IndicatorId "xxyy"))
xxx6 = dataset "xxx3" "xxyy" "xxyy" (WeightedBy (IndicatorId "xxyy"))
xxx5 = dataset "xxx4" "xxyy" "xxyy" (WeightedBy (IndicatorId "xxyy"))
xxx4 = dataset "xxx5" "xxyy" "xxyy" (WeightedBy (IndicatorId "xxx6"))
xxx33 = dataset "xxx7" "xxyy" "xxyy" (WeightedBy (IndicatorId "xxx0"))
xxx2 = dataset "xxx8" "xxyy" "xxyy" (WeightedBy (IndicatorId "xxx9"))

populationWeighted = WeightedBy (dsIndicator population)
surfaceWeighted = WeightedBy (dsIndicator surfaceArea)
arableWeighted = WeightedBy (IndicatorId "AG.LND.ARBL.HA")
