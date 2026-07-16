-----------------------------------------------------------------------------
--
-- Module      :   Dataset

-- the local names for datasets 
-- plus other properties to be set 

-----------------------------------------------------------------------------

module BaseTest.Dataset
     where

import Data.List (find)
import UniformBase 

import R4C.Model




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

datasets :: [Dataset]
datasets =
    [ population
    , surfaceArea
    ]

lookupIndicator :: IndicatorId -> Maybe Dataset
lookupIndicator iid =
    find (\i -> dsIndicator i == iid) datasets
    