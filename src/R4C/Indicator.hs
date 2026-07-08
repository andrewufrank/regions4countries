-----------------------------------------------------------------------------
--
-- Module      :   Indicator

-- Indicators are the things observed 
-- in the worldbank also called "series"
-- this file must be extended with every dataset added 

-----------------------------------------------------------------------------

module R4C.Indicator
    ( population
    , surfaceArea
    , indicators
    , lookupIndicator
    ) where

import Data.List (find)

import R4C.Model

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
    