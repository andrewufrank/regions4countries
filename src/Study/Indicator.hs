-----------------------------------------------------------------------------
--
-- Module      :   Indicator

-- Indicators are the things observed 
-- in the worldbank also called "series"
-- this file must be extended with every dataset added 

-----------------------------------------------------------------------------

module Study.Indicator
      where

import Data.List (find)

import R4C.Model
import GHC.IO.StdHandles (withFileBlocking)

fertilityRate = Indicator 
    {indicatorId = IndicatorId "SP.DYN.TFRT.IN"
    , indicatorName = "Fertilitaetsrate"
    , aggregation = WeightedBy (indicatorId population) }   -- sollted weighted by female population sein  
-- Total fertility rate represents the number of children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance with age-specific fertility rates of the specified year. 

migrationNet :: Indicator 
migrationNet = Indicator 
    { indicatorId = IndicatorId "SM.POP.NETM"
    , indicatorName = "Netto Migration (Persons)"
    , aggregation =  Sum}
-- Net migration is the net total of migrants during the period, that is, the number of immigrants minus the number of emigrants, including both citizens and noncitizens.

population :: Indicator
population =
    Indicator
        { indicatorId   = IndicatorId "SP.POP.TOTL"
        , indicatorName = "Population, total"
        , aggregation   = Sum
        }

surfaceArea :: Indicator
surfaceArea =
    Indicator
        { indicatorId   = IndicatorId "AG.SRF.TOTL.K2"
        , indicatorName = "Surface area"
        , aggregation   = Sum
        }

indicators :: [Indicator]
indicators =
    [ population
    , surfaceArea
    ]

lookupIndicator :: IndicatorId -> Maybe Indicator
lookupIndicator iid =
    find (\i -> indicatorId i == iid) indicators
    