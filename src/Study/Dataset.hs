-----------------------------------------------------------------------------
--
-- Module      :   Dataset

-- my description of the datasets inserted 

-----------------------------------------------------------------------------

module Study.Dataset
      where

import Data.List (find)

import R4C.Model
import GHC.IO.StdHandles (withFileBlocking)

energyConsum = Dataset 
    { dsIndicator = IndicatorId "SM.POP.NETM"
    , dsName = "Energie Verbrauch"
    , dsAggregation =  Sum}
-- Energy use refers to use of primary energy before transformation to other end-use fuels, which is equal to indigenous production plus imports and stock changes, minus exports and fuels supplied to ships and aircraft engaged in international transport.

waterConsum = Dataset 
    { dsIndicator = IndicatorId "ER.H2O.FWTL.ZS"
    , dsName = "Wasserverbrauch (Prozent des Verfuegbaren)"
    , dsAggregation =  Sum}
-- Annual freshwater withdrawals refer to total water withdrawals, not counting evaporation losses from storage basins. Withdrawals also include water from desalination plants in countries where they are a significant source. Withdrawals can exceed 100 percent of total renewable resources where extraction from nonrenewable aquifers or desalination plants is considerable or where there is significant water reuse. Withdrawals for agriculture and industry are total withdrawals for irrigation and livestock production and for direct industrial use (including withdrawals for cooling thermoelectric plants). Withdrawals for domestic uses include drinking water, municipal use or supply, and use for public services, commercial establishments, and homes. Data are for the most recent year available for 1987-2002.


agrarland = Dataset  -- percent of total surface
    { dsIndicator = IndicatorId "AG.LND.AGRI.ZS"
    , dsName = "Landwirtschaftsland"
    , dsAggregation =  WeightedBy $ IndicatorId "AG.SRF.TOTL.K2"}


cerealProduction = Dataset 
    { dsIndicator = IndicatorId "AG.PRD.CREL.MT"
    , dsName = "Getreideproduktion"
    , dsAggregation =  Sum}


agriculturalLand = Dataset 
    { dsIndicator = IndicatorId "AG.LND.ARBL.HA"
    , dsName = "Landwirtschaftsland"
    , dsAggregation =  Sum}
-- Arable land (in hectares) includes land defined by the FAO as land under temporary crops (double-cropped areas are counted once), temporary meadows for mowing or for pasture, land under market or kitchen gardens, and land temporarily fallow. Land abandoned as a result of shifting cultivation is excluded.
agriculturalLandPC = Dataset 
    { dsIndicator = IndicatorId "AG.LND.ARBL.HA.PC"
    , dsName = "Landwirtschaftsland ha per capita"
    , dsAggregation =  WeightedBy (dsIndicator population)} 

ferilizerConsum2 = Dataset 
    { dsIndicator = IndicatorId "AG.CON.FERT.PT.ZS"
    , dsName = "Duengerverbrauch pro ha arable land  )"
    , dsAggregation =  WeightedBy $ IndicatorId "AG.LND.ARBL.HA"}
-- Fertilizer consumption measures the quantity of plant nutrients used per unit of arable land. Fertilizer products cover nitrogenous, potash, and phosphate fertilizers (including ground rock phosphate). Traditional nutrients--animal and plant manures--are not included. For the purpose of data dissemination, FAO has adopted the concept of a calendar year (January to December). Some countries compile fertilizer data on a calendar year basis, while others are on a split-year basis. Arable land includes land defined by the FAO as land under temporary crops (double-cropped areas are counted once), temporary meadows for mowing or for pasture, land under market or kitchen gardens, and land temporarily fallow. Land abandoned as a result of shifting cultivation is excluded.

ferilizerConsum = Dataset 
    { dsIndicator = IndicatorId "AG.CON.FERT.ZS"
    , dsName = "Duengerverbrauch"
    , dsAggregation =  Sum}
-- Fertilizer consumption measures the quantity of plant nutrients and is calculated as production plus imports minus exports. Fertilizer products cover nitrogenous, potash, and phosphate fertilizers (including ground rock phosphate). Traditional nutrients--animal and plant manures--are not included. Because some chemical compounds used for fertilizers have other industrial applications, the consumption data may overstate the quantity available for crops. Fertilizer consumption as a share of production shows the agriculture sector's vulnerability to import and energy price fluctuation. Most fertilizers that are commonly used in agriculture contain the three basic plant nutrients-nitrogen, phosphorus, and potassium. Some fertilizers also contain certain micronutrients such as zinc and other metals that are necessary for plant growth. Materials that are applied to the land primarily to enhance soil characteristics (rather than as plant food) are commonly referred to as soil amendments.


migrationNet = Dataset 
    { dsIndicator = IndicatorId "SM.POP.NETM"
    , dsName = "Netto Migration (Persons)"
    , dsAggregation =  Sum}
-- Net migration is the net total of migrants during the period, that is, the number of immigrants minus the number of emigrants, including both citizens and noncitizens.


fertilityRate = Dataset 
    {dsIndicator = IndicatorId "SP.DYN.TFRT.IN"
    , dsName = "Fertilitaetsrate"
    , dsAggregation = WeightedBy (dsIndicator population) }   -- sollted weighted by female population sein  
-- Total fertility rate represents the number of children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance with age-specific fertility rates of the specified year. 

getreideErtrag :: Dataset 
getreideErtrag = Dataset 
    { dsIndicator = IndicatorId "AG.YLD.CREL.KG"
    , dsName = "Getreideertrag"
    , dsAggregation =  Sum}
-- Cereal yield, measured as kilograms per hectare of harvested land, includes wheat, rice, maize, barley, oats, rye, millet, sorghum, buckwheat, and mixed grains. Production data on cereals relate to crops harvested for dry grain only. Cereal crops harvested for hay or harvested green for food, feed, or silage and those used for grazing are excluded. The FAO allocates production data to the calendar year in which the bulk of the harvest took place. Most of a crop harvested near the end of a year will be used in the following year.

population :: Dataset
population =
    Dataset
        { dsName = "Population"
        , dsIndicator   = IndicatorId   "SP.POP.TOTL"  
        , dsAggregation   = Sum
        }

surfaceArea :: Dataset
surfaceArea =
    Dataset
        { dsIndicator   = IndicatorId "AG.SRF.TOTL.K2"
        , dsName = "Surface area"
        , dsAggregation   = Sum
        }
grossNatProd :: Dataset 
grossNatProd = Dataset 
    { dsIndicator = IndicatorId "NY.GNP.ATLS.CD"
    , dsName = "GNP"
    , dsAggregation = Sum
    }
-- Gross national income is the total income earned by all residents within an economic territory during an accounting period. It is equal to gross domestic product plus earned income receivable from abroad minus earned income payable abroad. This figure is converted to U.S. dollars using the World Bank Atlas method. GNI, calculated in national currency, is usually converted to U.S. dollars at official exchange rates for comparisons across economies, although an alternative rate is used when the official exchange rate is judged to diverge by an exceptionally large margin from the rate actually applied in international transactions. To smooth fluctuations in prices and exchange rates, a special Atlas method of conversion is used by the World Bank. This applies a conversion factor that averages the exchange rate for a given year and the two preceding years, adjusted for differences in rates of inflation between the country, and through 2000, the G-5 countries (France, Germany, Japan, the United Kingdom, and the United States). From 2001, these countries include the Euro area, Japan, the United Kingdom, and the United States. This indicator is expressed in current prices, meaning no adjustment has been made to account for price changes over time. This indicator is expressed in United States dollars.

gnpPPpc :: Dataset 
gnpPPpc = Dataset 
    { dsIndicator = IndicatorId "NY.GDP.PCAP.PP.CD"
    , dsName = "GNP PP per Capita"
    , dsAggregation = WeightedBy (dsIndicator population)
    }


indicators :: [Dataset]
indicators =
    [ population
    , surfaceArea
    ]

lookupIndicator :: IndicatorId -> Maybe Dataset
lookupIndicator iid =
    find (\i -> dsIndicator i == iid) indicators
    