-- | The editable, generated registry for this study.
module Study.Descriptor where

import Data.List (find)

import R4C.Model

population :: Dataset
population =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.POP.TOTL"}
        , dsShortName = "Population"
        , dsName = "Population, total"
        , dsDefinition = "Total population  "
        , dsUnit = "P"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = True
        , dsLastYear = Just (Year 2024)
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN), uri:"
             
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
        , dsScale = Unit
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
        , dsDecimals = 0
        , dsScale = Unit
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
        , dsShortName = "Landwirtschaftsland"
        , dsName = "Surface area (sq. km)"
        , dsDefinition =
            " ."
        , dsUnit = "km\178"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Unit
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
        , dsUnit = "TODO"
        , dsAggregation = WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "World Population Prospects, United Nations (UN) (ESTAT)"
        }

grossNatProd :: Dataset
grossNatProd =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "NY.GNP.ATLS.CD"}
        , dsShortName = "GNP"
        , dsName = "GNI, Atlas method (current US$)"
        , dsDefinition =
            "Gross national income  "
        , dsUnit = "TODO"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            " World Bank (WB)"
        }


-- gnpPP :: Dataset
gnpPPpc =
    Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "NY.GNP.MKTP.PP.KD"}  --NY.GDP.PCAP.PP.CD"}
        , dsShortName = "GNP PP  "
        , dsName = "GDP  PPP (current international $)"
        , dsDefinition =   ""
        , dsUnit = "PP$"
        , dsAggregation = Sum  
        , dsDecimals = 0
        , dsScale = Mega
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

xxx2 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.DYN.TFRT.IN"}
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

xxx3 = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.DYN.TFRT.IN"}
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
        { dsIndicator = IndicatorId {unIndicatorId = "SP.DYN.TFRT.IN"}
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
        { dsIndicator = IndicatorId {unIndicatorId = "SP.DYN.TFRT.IN"}
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

usableAreaPerCapita = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.DYN.TFRT.IN"}
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

surfacePerCapita = Dataset
        { dsIndicator = IndicatorId {unIndicatorId = "SP.xxx.IN"}
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

