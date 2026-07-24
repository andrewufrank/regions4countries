-- | The editable, generated registry for this study.
module Study.Dataset where

import Data.List (find)

import R4C.Model
-- import qualified Study.Dataset2 as Previous

-- BEGIN GENERATED DATASETS
energyConsum :: Dataset
energyConsum =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SM.POP.NETM"}
        , dsShortName = "Energie Verbrauch"
        , dsName = "Net migration"
        , dsDefinition =
            "Net migration is the net total of migrants during the"
            <> " period, that is, the number of immigrants minus the number"
            <> " of emigrants, including both citizens and noncitizens."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN),"
            <> " publisher: UN Population Division"
        }

waterConsum :: Dataset
waterConsum =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "ER.H2O.FWTL.ZS"}
        , dsShortName = "Wasserverbrauch"
        , dsName = "Annual freshwater withdrawals, total (% of internal resources)"
        , dsDefinition =
            "Annual freshwater withdrawals refer to total water"
            <> " withdrawals, not counting evaporation losses from storage"
            <> " basins. Withdrawals also include water from desalination"
            <> " plants in countries where they are a significant source."
            <> " Withdrawals can exceed 100 percent of total renewable"
            <> " resources where extraction from nonrenewable aquifers or"
            <> " desalination plants is considerable or where there is"
            <> " significant water reuse. Withdrawals for agriculture and"
            <> " industry are total withdrawals for irrigation and"
            <> " livestock production and for direct industrial use"
            <> " (including withdrawals for cooling thermoelectric plants)."
            <> " Withdrawals for domestic uses include drinking water,"
            <> " municipal use or supply, and use for public services,"
            <> " commercial establishments, and homes. Data are for the"
            <> " most recent year available for 1987-2002."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "AQUASTAT - FAO's Global Information System on Water and"
            <> " Agriculture, Food and Agriculture Organization of the"
            <> " United Nations (FAO), uri:"
            <> " https://data.apps.fao.org/aquastat/, publisher: Food and"
            <> " Agriculture Organization of the United Nations (FAO), date"
            <> " accessed: 20240529"
        }

agrarland :: Dataset
agrarland =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.LND.AGRI.ZS"}
        , dsShortName = "Landwirtschaftsland"
        , dsName = "Agricultural land (% of land area)"
        , dsDefinition =
            "Agricultural land refers to the share of land area that is"
            <> " arable, under permanent crops, and under permanent"
            <> " pastures. Arable land includes land defined by the FAO as"
            <> " land under temporary crops (double-cropped areas are"
            <> " counted once), temporary meadows for mowing or for"
            <> " pasture, land under market or kitchen gardens, and land"
            <> " temporarily fallow. Land abandoned as a result of shifting"
            <> " cultivation is excluded. Land under permanent crops is"
            <> " land cultivated with crops that occupy the land for long"
            <> " periods and need not be replanted after each harvest, such"
            <> " as cocoa, coffee, and rubber. This category includes land"
            <> " under flowering shrubs, fruit trees, nut trees, and vines,"
            <> " but excludes land under trees grown for wood or timber."
            <> " Permanent pasture is land used for five or more years for"
            <> " forage, including natural and cultivated crops."
        , dsUnit = "TODO"
        , dsAggregation = WeightedBy (IndicatorId "AG.SRF.TOTL.K2")
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

cerealProduction :: Dataset
cerealProduction =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.PRD.CREL.MT"}
        , dsShortName = "Getreideproduktion"
        , dsName = "Cereal production (metric tons)"
        , dsDefinition =
            "Production data on cereals relate to crops harvested for"
            <> " dry grain only. Cereal crops harvested for hay or"
            <> " harvested green for food, feed, or silage and those used"
            <> " for grazing are excluded."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), uri:"
            <> " https://www.fao.org/faostat/en/#data/QCL, publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

agriculturalLand :: Dataset
agriculturalLand =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.LND.ARBL.HA"}
        , dsShortName = "Landwirtschaftsland"
        , dsName = "Arable land (hectares)"
        , dsDefinition =
            "Arable land (in hectares) includes land defined by the FAO"
            <> " as land under temporary crops (double-cropped areas are"
            <> " counted once), temporary meadows for mowing or for"
            <> " pasture, land under market or kitchen gardens, and land"
            <> " temporarily fallow. Land abandoned as a result of shifting"
            <> " cultivation is excluded."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

agriculturalLandPC :: Dataset
agriculturalLandPC =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.LND.ARBL.HA.PC"}
        , dsShortName = "Landwirtschaftsland ha per capita"
        , dsName = "Arable land (hectares per person)"
        , dsDefinition =
            "Arable land (hectares per person) includes land defined by"
            <> " the FAO as land under temporary crops (double-cropped"
            <> " areas are counted once), temporary meadows for mowing or"
            <> " for pasture, land under market or kitchen gardens, and"
            <> " land temporarily fallow. Land abandoned as a result of"
            <> " shifting cultivation is excluded."
        , dsUnit = "TODO"
        , dsAggregation = WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

ferilizerConsum2 :: Dataset
ferilizerConsum2 =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.CON.FERT.PT.ZS"}
        , dsShortName = "Duengerverbrauch pro ha arable land"
        , dsName = "Fertilizer consumption (% of fertilizer production)"
        , dsDefinition =
            "Fertilizer consumption measures the quantity of plant"
            <> " nutrients and is calculated as production plus imports"
            <> " minus exports. Fertilizer products cover nitrogenous,"
            <> " potash, and phosphate fertilizers (including ground rock"
            <> " phosphate). Traditional nutrients--animal and plant"
            <> " manures--are not included. Because some chemical compounds"
            <> " used for fertilizers have other industrial applications,"
            <> " the consumption data may overstate the quantity available"
            <> " for crops. Fertilizer consumption as a share of production"
            <> " shows the agriculture sector's vulnerability to import and"
            <> " energy price fluctuation. Most fertilizers that are"
            <> " commonly used in agriculture contain the three basic plant"
            <> " nutrients-nitrogen, phosphorus, and potassium. Some"
            <> " fertilizers also contain certain micronutrients such as"
            <> " zinc and other metals that are necessary for plant growth."
            <> " Materials that are applied to the land primarily to"
            <> " enhance soil characteristics (rather than as plant food)"
            <> " are commonly referred to as soil amendments."
        , dsUnit = "TODO"
        , dsAggregation = WeightedBy (IndicatorId "AG.LND.ARBL.HA")
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

ferilizerConsum :: Dataset
ferilizerConsum =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.CON.FERT.ZS"}
        , dsShortName = "Duengerverbrauch"
        , dsName = "Fertilizer consumption (kilograms per hectare of arable land)"
        , dsDefinition =
            "Fertilizer consumption measures the quantity of plant"
            <> " nutrients used per unit of arable land. Fertilizer"
            <> " products cover nitrogenous, potash, and phosphate"
            <> " fertilizers (including ground rock phosphate). Traditional"
            <> " nutrients--animal and plant manures--are not included. For"
            <> " the purpose of data dissemination, FAO has adopted the"
            <> " concept of a calendar year (January to December). Some"
            <> " countries compile fertilizer data on a calendar year"
            <> " basis, while others are on a split-year basis. Arable land"
            <> " includes land defined by the FAO as land under temporary"
            <> " crops (double-cropped areas are counted once), temporary"
            <> " meadows for mowing or for pasture, land under market or"
            <> " kitchen gardens, and land temporarily fallow. Land"
            <> " abandoned as a result of shifting cultivation is excluded."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

migrationNet :: Dataset
migrationNet =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SM.POP.NETM"}
        , dsShortName = "Netto Migration (Persons)"
        , dsName = "Net migration"
        , dsDefinition =
            "Net migration is the net total of migrants during the"
            <> " period, that is, the number of immigrants minus the number"
            <> " of emigrants, including both citizens and noncitizens."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN),"
            <> " publisher: UN Population Division"
        }

fertilityRate :: Dataset
fertilityRate =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.DYN.TFRT.IN"}
        , dsShortName = "Fertilitaetsrate"
        , dsName = "Fertility rate, total (births per woman)"
        , dsDefinition =
            "Total fertility rate represents the number of children"
            <> " that would be born to a woman if she were to live to the"
            <> " end of her childbearing years and bear children in"
            <> " accordance with age-specific fertility rates of the"
            <> " specified year."
        , dsUnit = "TODO"
        , dsAggregation = WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN),"
            <> " publisher: UN Population Division; Statistical databases"
            <> " and publications from national statistical offices,"
            <> " National Statistical Offices (NSOs); Demographic"
            <> " Statistics, Eurostat (ESTAT)"
        }

getreideErtrag :: Dataset
getreideErtrag =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.YLD.CREL.KG"}
        , dsShortName = "Getreideertrag"
        , dsName = "Cereal yield (kg per hectare)"
        , dsDefinition =
            "Cereal yield, measured as kilograms per hectare of"
            <> " harvested land, includes wheat, rice, maize, barley, oats,"
            <> " rye, millet, sorghum, buckwheat, and mixed grains."
            <> " Production data on cereals relate to crops harvested for"
            <> " dry grain only. Cereal crops harvested for hay or"
            <> " harvested green for food, feed, or silage and those used"
            <> " for grazing are excluded. The FAO allocates production"
            <> " data to the calendar year in which the bulk of the harvest"
            <> " took place. Most of a crop harvested near the end of a"
            <> " year will be used in the following year."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), uri:"
            <> " https://www.fao.org/faostat/en/#data/QCL, publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

population :: Dataset
population =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.POP.TOTL"}
        , dsShortName = "Population"
        , dsName = "Population, total"
        , dsDefinition =
            "Total population is based on the de facto definition of"
            <> " population, which counts all residents regardless of legal"
            <> " status or citizenship. The values shown are midyear"
            <> " estimates."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN), uri:"
            <> " https://population.un.org/wpp/, publisher: UN Population"
            <> " Division; Statistical databases and publications from"
            <> " national statistical offices, National Statistical Offices"
            <> " (NSOs), uri: https://unstats.un.org/home/nso_sites/,"
            <> " publisher: National Statistical Offices; Eurostat:"
            <> " Demographic Statistics, Eurostat (ESTAT), uri:"
            <> " https://ec.europa.eu/eurostat/data/database?node_code=earn_ses_monthly,"
            <> " publisher: Eurostat; Population and Vital Statistics"
            <> " Report (various years), United Nations (UN), uri:"
            <> " https://unstats.un.org, publisher: UN Statistics Division"
        }

surfaceArea :: Dataset
surfaceArea =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.SRF.TOTL.K2"}
        , dsShortName = "Surface area"
        , dsName = "Surface area (sq. km)"
        , dsDefinition =
            "Surface area is a country's total area, including areas"
            <> " under inland bodies of water and some coastal waterways."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

grossNatProd :: Dataset
grossNatProd =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "NY.GNP.ATLS.CD"}
        , dsShortName = "GNP"
        , dsName = "GNI, Atlas method (current US$)"
        , dsDefinition =
            "Gross national income is the total income earned by all"
            <> " residents within an economic territory during an"
            <> " accounting period. It is equal to gross domestic product"
            <> " plus earned income receivable from abroad minus earned"
            <> " income payable abroad. This figure is converted to U.S."
            <> " dollars using the World Bank Atlas method. GNI, calculated"
            <> " in national currency, is usually converted to U.S. dollars"
            <> " at official exchange rates for comparisons across"
            <> " economies, although an alternative rate is used when the"
            <> " official exchange rate is judged to diverge by an"
            <> " exceptionally large margin from the rate actually applied"
            <> " in international transactions. To smooth fluctuations in"
            <> " prices and exchange rates, a special Atlas method of"
            <> " conversion is used by the World Bank. This applies a"
            <> " conversion factor that averages the exchange rate for a"
            <> " given year and the two preceding years, adjusted for"
            <> " differences in rates of inflation between the country, and"
            <> " through 2000, the G-5 countries (France, Germany, Japan,"
            <> " the United Kingdom, and the United States). From 2001,"
            <> " these countries include the Euro area, Japan, the United"
            <> " Kingdom, and the United States. This indicator is"
            <> " expressed in current prices, meaning no adjustment has"
            <> " been made to account for price changes over time. This"
            <> " indicator is expressed in United States dollars."
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "Country official statistics, National Statistical"
            <> " Organizations and/or Central Banks; National Accounts data"
            <> " files, Organisation for Economic Co-operation and"
            <> " Development (OECD); Staff estimates, World Bank (WB)"
        }

gnpPPpc :: Dataset
gnpPPpc =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "NY.GDP.PCAP.PP.CD"}
        , dsShortName = "GNP PP per Capita"
        , dsName = "GDP per capita, PPP (current international $)"
        , dsDefinition =
            "This indicator provides values for gross domestic product"
            <> " (GDP) per person expressed in current international"
            <> " dollars, converted by purchasing power parities (PPPs)."
            <> " PPPs account for the different price levels across"
            <> " countries and thus PPP-based comparisons of economic"
            <> " output are more appropriate for comparing the output of"
            <> " economies and the average material well-being of their"
            <> " inhabitants than exchange-rate based comparisons. Gross"
            <> " domestic product is the total income earned through the"
            <> " production of goods and services in an economic territory"
            <> " during an accounting period. It can be measured in three"
            <> " different ways: using either the expenditure approach, the"
            <> " income approach, or the production approach. This series"
            <> " has been linked to produce a consistent time series to"
            <> " counteract breaks in series over time due to changes in"
            <> " base years, source data and methodologies. Thus, it may"
            <> " not be comparable with other national accounts series in"
            <> " the database for historical years. The core indicator has"
            <> " been divided by the general population to achieve a per"
            <> " capita estimate. This indicator is expressed in current"
            <> " prices, meaning no adjustment has been made to account for"
            <> " price changes over time. The PPP conversion factor is a"
            <> " currency conversion factor and a spatial price deflator."
            <> " PPPs convert different currencies to a common currency"
            <> " and, in the process of conversion, equalize their"
            <> " purchasing power by eliminating the differences in price"
            <> " levels between countries, thereby allowing volume or"
            <> " output comparisons of GDP and its expenditure components."
        , dsUnit = "TODO"
        , dsAggregation = WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "International Comparison Program (ICP), World Bank (WB),"
            <> " uri: https://www.worldbank.org/en/programs/icp/data, note:"
            <> " This information is for ICP\8217s PPPs utilized in WDI,"
            <> " publisher: International Comparison Program (ICP), date"
            <> " accessed: May 30, 2024, date published: May 30, 2024; The"
            <> " Eurostat PPP Programme, Eurostat (ESTAT), uri:"
            <> " https://ec.europa.eu/eurostat/databrowser/explore/all/all_themes,"
            <> " publisher: Eurostat; The OECD PPP Programme, Organisation"
            <> " for Economic Co-operation and Development (OECD), uri:"
            <> " https://data-explorer.oecd.org/, publisher: OECD; Staff"
            <> " estimates, World Bank (WB); National Accounts data files,"
            <> " Organisation for Economic Co-operation and Development"
            <> " (OECD); World Economic Outlook database, International"
            <> " Monetary Fund (IMF)"
        }

-- END GENERATED DATASETS

namedDatasets :: [(String, Dataset)]
namedDatasets =
    [ ("energyConsum", energyConsum), ("waterConsum", waterConsum)
    , ("agrarland", agrarland), ("cerealProduction", cerealProduction)
    , ("agriculturalLand", agriculturalLand)
    , ("agriculturalLandPC", agriculturalLandPC)
    , ("ferilizerConsum2", ferilizerConsum2)
    , ("ferilizerConsum", ferilizerConsum), ("migrationNet", migrationNet)
    , ("fertilityRate", fertilityRate), ("getreideErtrag", getreideErtrag)
    , ("population", population), ("surfaceArea", surfaceArea)
    , ("grossNatProd", grossNatProd), ("gnpPPpc", gnpPPpc)
    ]

datasets :: [Dataset]
datasets =
    [ waterConsum, agrarland, cerealProduction, agriculturalLand
    , agriculturalLandPC, ferilizerConsum2, ferilizerConsum, migrationNet
    , fertilityRate, getreideErtrag, population, surfaceArea, grossNatProd
    , gnpPPpc
    ]

lookupDataset :: IndicatorId -> Maybe Dataset
lookupDataset iid = find ((== iid) . dsIndicator) datasets
