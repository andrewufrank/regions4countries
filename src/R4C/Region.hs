-----------------------------------------------------------------------------
--
-- Module      :   Region 
-- the definitions of the regions 
-----------------------------------------------------------------------------

module R4C.Region
  ( regionMembers
  , countriesInRegion
  ) where

import R4C.Model

regionMembers :: [(RegionId, [CountryId])]
regionMembers =
  [ (RegionId "EU",
      [ CountryId "AUT"
      , CountryId "BEL"
      , CountryId "BGR"
      , CountryId "HRV"
      , CountryId "CYP"
      , CountryId "CZE"
      , CountryId "DNK"
      , CountryId "EST"
      , CountryId "FIN"
      , CountryId "FRA"
      , CountryId "DEU"
      , CountryId "GRC"
      , CountryId "HUN"
      , CountryId "IRL"
      , CountryId "ITA"
      , CountryId "LVA"
      , CountryId "LTU"
      , CountryId "LUX"
      , CountryId "MLT"
      , CountryId "NLD"
      , CountryId "POL"
      , CountryId "PRT"
      , CountryId "ROU"
      , CountryId "SVK"
      , CountryId "SVN"
      , CountryId "ESP"
      , CountryId "SWE"
      ])
  , (RegionId "G7",
      [ CountryId "CAN"
      , CountryId "FRA"
      , CountryId "DEU"
      , CountryId "ITA"
      , CountryId "JPN"
      , CountryId "GBR"
      , CountryId "USA"
      ])
  ]

countriesInRegion :: RegionId-> [CountryId]
countriesInRegion r =
  case lookup r regionMembers of
    Just cs -> cs
    Nothing -> []