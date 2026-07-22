-----------------------------------------------------------------------------
--
-- Module      :   Region 
-- the definitions of the regions 
-----------------------------------------------------------------------------

module BaseTest.Region
    where

import R4C.Model
import Database.SQLite.Simple  -- for debug
import UniformBase
-- import BaseTest.Indicator


regionMembers :: RegionMembers  -- [(RegionId, [CountryId])]  -- [(RegionId, CountryId)]
regionMembers =  
  [ mk "G7"
        ["CAN","FRA","DEU","ITA","JPN","GBR","USA"]

  , mk "EU"
        ["AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA"
        ,"DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD"
        ,"POL","PRT","ROU","SVK","SVN","ESP","SWE"]
  ]
  where
    -- mk :: Text -> [Text] -> [(RegionId, CountryId)]
    -- mk r = map (\c -> (RegionId r, CountryId c))
    mk :: Text -> [Text] -> (RegionId, [CountryId])
    mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)


regionNames :: [TerryName RegionId] 
regionNames = [Terry (RegionId "G7") "Gruppe 7"
              , Terry (RegionId "EU") "Europ. Union" 
              ]








