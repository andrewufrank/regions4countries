-----------------------------------------------------------------------------
--
-- Module      :   Dataset

-- the local names for datasets 
-- plus other properties to be set 

-----------------------------------------------------------------------------

module BaseTest.Dataset1
     where

import Data.List (find)
import UniformBase 

import R4C.Model 

-- data Dataset1 = Dataset
--     { dsIndicator          :: IndicatorId
--     , dsShortName          :: Text
--     , dsName               :: Text
--     , dsDefinition         :: Text
--     , dsUnit               :: Text
--     , dsAggregation        :: Aggregation
--     , dsScale               :: Scale
--     , dsDecimals           :: Int
--     , dsExtensive          :: Bool
--     , dsLastYear           :: Maybe Year
--     , dsSourceOrganization :: Text
--     }
--     deriving (Eq, Ord, Show)

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
        , dsUnit = "P"  -- for Person
        , dsAggregation = Sum  -- sum means extensive
        , dsDecimals = 0
        , dsScale = Mega
        , dsExtensive = True
        , dsLastYear = Just (Year 2024)
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
        , dsUnit = "km²"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Mega
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "FAO electronic files and web site, Food and Agriculture"
            <> " Organization of the United Nations (FAO), publisher: Food"
            <> " and Agriculture Organization of the United Nations (FAO)"
        }


-- datasets :: [Dataset]
-- datasets =
--     [ population
--     , surfaceArea
--     ]

-- lookupIndicator :: IndicatorId -> Maybe Dataset
-- lookupIndicator iid =
--     find (\i -> dsIndicator i == iid) datasets
    