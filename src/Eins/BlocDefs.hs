-----------------------------------------------------------------------------
--
-- Module      :   the definitions for Bloc (or super-Bloc)
-- the definitions of the blocks 
-----------------------------------------------------------------------------

module Eins.BlocDefs
  where

import R4C.Model
import UniformBase
import Data.List (nub)
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator

blocsList2 :: [BlocId]
blocsList2 = nub $ map fst blocMembers2

blocMembers2 :: BlocMembers  -- [(BlocId, CountryId)]
blocMembers2 = 
  [
    mk "Washington" 
      ["USCAN", "SAMERICA"]

  , mk "Bruessel"
      [ "EUROPE", "NORTH_AFRICA"] -- , "RUSSIA", "CENTRAL_ASIA"

  , mk "Moskau" ["RUSSIA", "CENTRAL_ASIA"]

  , mk "Peking"
      ["CHN", "GULF", "FAREAST"]

  , mk "Dehli"
      ["INDIA", "SOUTH_ASIA"]

  , mk "Tokio"
      ["JAPAN"]
  , mk "Other" ["ANZ", "SUBSAHARA"]


  ]
  where
    mk :: Text -> [Text] -> (BlocId, [RegionId])
    mk r cs = (BlocId r, map (\c -> (RegionId c)) cs)

 
blocNames2 :: [TerryName BlocId] --  [Bloc]
blocNames2 =
  [ Terry (BlocId "Bruessel")        "Bruessel"
  , Terry (BlocId "Moskau")         "Moskau"
  , Terry (BlocId "Peking")         "Peking"
  , Terry (BlocId "Washington")     "Washington"
  , Terry (BlocId "Dehli")          "Dehli"
  , Terry (BlocId "Tokio")          "Tokio"
  , Terry (BlocId "Other")          "Other"
  ]
 

blocOrder2 = map terryId blocNames2 

