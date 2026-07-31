-- | The editable, generated registry for this study.
module Eins.Descriptor where

import Data.List (find)

import R4C.Model

sqkm = "km\178"

population :: Dataset
population =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.POP.TOTL"}
        , dsShortName = "Bevölkerung"
        , dsName = "Population, total"
        , dsDefinition = "Total population  "
        , dsUnit = "P"
        , dsAggregation = Sum
        , dsDecimals = 3
        , dsScale = Mega
        , dsExtensive = True
        , dsLastYear = Just (Year 2024)
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN), uri:"
             
        }
migrationNet :: Dataset
migrationNet =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SM.POP.NETM"}
        , dsShortName = "Netto Migration"
        , dsName = "Net migration per Year"
        , dsDefinition =
            "Net migration is the net total of migrants during the"
            <> " period, that is, the number of immigrants minus the number"
            <> " of emigrants, including both citizens and noncitizens."
        , dsUnit = "P/y"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN),"
            <> " publisher: UN Population Division"
        }

surfaceArea :: Dataset
surfaceArea =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.SRF.TOTL.K2"}
        , dsShortName = "Flaeche"
        , dsName = "Surface area (sq. km)"
        , dsDefinition =
            "Surface area is a country's total area, including areas"
            <> " under inland bodies of water and some coastal waterways."
        , dsUnit = "km\178"
        , dsAggregation = Sum
        , dsDecimals = 3
        , dsScale = Mega
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }

arableLand :: Dataset
arableLand =
    Dataset
        { dsIndicator = IndicatorId "zzz"
        , dsShortName = "Landwirtschaft"
        , dsName = "Surface area (sq. km)"
        , dsDefinition =
            " ."
        , dsUnit = "km\178"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO  "
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
        , dsUnit = "P/woman"
        , dsAggregation = WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 2
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN) (ESTAT)"
        }

-- grossNatProd :: Dataset
gnp =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "NY.GNP.ATLS.CD"}
        , dsShortName = "GNP"
        , dsName = "GNI, Atlas method (current US$)"
        , dsDefinition =
            "Gross national income  "
        , dsUnit = "US$"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Giga
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            " World Bank (WB)"
        }


gnpPP =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "NY.GNP.MKTP.PP.KD"}  --NY.GDP.PCAP.PP.CD"}
        , dsShortName = "GNP PP  "
        , dsName = "GDP  PPP (current international $)"
        , dsDefinition =   ""
        , dsUnit = "PP$"
        , dsAggregation = Sum  
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = True  
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "International Comparison Program (ICP), World Bank (WB "
         }

gdpPPpc = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "NY.GDP.PCAP.PP.CD"}
        , dsShortName = "GDP per capita"
        , dsName = "GDP_PP per capita"
        , dsDefinition =
            "Txxx"
        , dsUnit = "PP$/P"
        , dsAggregation = WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

agriPercent = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.LND.AGRI.ZS"}
        , dsShortName = "Landwirtschaft"
        , dsName = "Agriculturall land percent"
        , dsDefinition =
            "Txxx"
        , dsUnit = "%"
        , dsAggregation = Sum -- WeightedBy (IndicatorId "xxx")
        , dsDecimals = 2
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

xxx3 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.LND.FRST.ZS"}
        , dsShortName = "Wald Anteil"
        , dsName = "Forest area percent"
        , dsDefinition =
            "Txxx"
        , dsUnit = "%"
        , dsAggregation = Sum -- WeightedBy (IndicatorId "xxx")
        , dsDecimals = 2
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

forest = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.LND.FRST.K2"}
        , dsShortName = "Wald"
        , dsName = "Forest area"
        , dsDefinition =
            "Txxx"
        , dsUnit = sqkm
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

urban = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "AG.LND.TOTL.UR.K2"}
        , dsShortName = "Urban"
        , dsName = "Urban land area"
        , dsDefinition =
            "Txxx"
        , dsUnit = sqkm
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

populationGrowthRate = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.POP.GROW"}
        , dsShortName = "Wachstum Bevoelkerung"
        , dsName = "population growth annual "
        , dsDefinition =
            "Txxx"
        , dsUnit = "%"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 2
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

surfaxc1ePerCapita = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "xxx"}
        , dsShortName = "xxx"
        , dsName = "xxx"
        , dsDefinition =
            "Txxx"
        , dsUnit = "xxx"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

usableAxreaPerCapita1 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "xxx"}
        , dsShortName = "xxx"
        , dsName = "xxx"
        , dsDefinition =
            "Txxx"
        , dsUnit = "xxx"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }
xxx6 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "xxx"}
        , dsShortName = "xxx"
        , dsName = "xxx"
        , dsDefinition =
            "Txxx"
        , dsUnit = "xxx"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }
xxx5 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "xxx"}
        , dsShortName = "xxx"
        , dsName = "xxx"
        , dsDefinition =
            "Txxx"
        , dsUnit = "xxx"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }
xxx4 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "xxx"}
        , dsShortName = "xxx"
        , dsName = "xxx"
        , dsDefinition =
            "Txxx"
        , dsUnit = "xxx"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

xxx33 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "xxx"}
        , dsShortName = "xxx"
        , dsName = "xxx"
        , dsDefinition =
            "Txxx"
        , dsUnit = "xxx"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }
xxx2 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "xxx"}
        , dsShortName = "xxx"
        , dsName = "xxx"
        , dsDefinition =
            "Txxx"
        , dsUnit = "xxx"
        , dsAggregation = WeightedBy (IndicatorId "xxx")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }