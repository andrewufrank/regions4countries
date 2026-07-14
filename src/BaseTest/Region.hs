-----------------------------------------------------------------------------
--
-- Module      :   Region 
-- the definitions of the regions 
-----------------------------------------------------------------------------

module BaseTest.Region
  ( regionMembers
--   , countriesInRegion
  ) where

import R4C.Model
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import BaseTest.Indicator

regionMembers :: [(RegionId, CountryId)]
regionMembers = concat
  [ mk "G7"
        ["CAN","FRA","DEU","ITA","JPN","GBR","USA"]

  , mk "EU"
        ["AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA"
        ,"DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD"
        ,"POL","PRT","ROU","SVK","SVN","ESP","SWE"]
  ]
  where
    -- mk :: Text -> [Text] -> [(RegionId, CountryId)]
    mk r = map (\c -> (RegionId r, CountryId c))









