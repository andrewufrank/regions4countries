-----------------------------------------------------------------------------
--
-- Module      :   Region 
-- the definitions of the regions 
-----------------------------------------------------------------------------

module Study.Region2
  where

import R4C.Model
import UniformBase
import Data.List (nub)
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator

regionsList :: [RegionId]
regionsList = nub $ map fst regionMembers

regionMembers :: RegionMembers  -- [(RegionId, CountryId)]
regionMembers = 
  [

  mk "SAMERICA"
      [ "ATG","ARG","ABW","BHS","BRB","BLZ","BOL","BRA","CHL","COL"
      , "CRI","CUB","DMA","DOM","ECU","SLV","GRD","GTM","GUY","HTI"
      , "HND","JAM","MEX","NIC","PAN","PRY","PER","KNA","LCA","VCT"
      , "SUR","TTO","URY","VEN"
      ]
  ,  mk "USCAN"
      ["USA", "CAN"]

  , mk "EUROPE"
      [ "ALB","AND","AUT","BLR","BEL","BIH","BGR","HRV","CYP","CZE"
      , "DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ITA"
      , "XKX","LVA","LIE","LTU","LUX","MLT","MDA","MCO","MNE","NLD"
      , "MKD","NOR","POL","PRT","ROU","SMR","SRB","SVK","SVN","ESP"
      , "SWE","CHE","TUR","UKR","GBR","VAT"
      ]

  , mk "RUSSIA"
      ["RUS"]

  , mk "CHINA"
      ["CHN"]

  , mk "INDIA"
      ["IND"]

  , mk "JAPAN"
      ["JPN"]

  , mk "FAREAST"
      [ "BRN","KHM","IDN","LAO","MYS","MNG","MMR"
      , "PRK","PHL","SGP","KOR","TWN","THA","VNM"
      ]

  , mk "CENTRAL_ASIA"
      ["UZB","KAZ","TJK","KGZ","TKM"]

  , mk "SOUTH_ASIA"
      ["PAK","BGD","AFG","NPL","LKA"]

  , mk "NORTH_AFRICA"
      ["DZA","EGY","LBY","MAR","SDN","TUN","ESH"]

  , mk "SUBSAHARA"
      [ "AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM"
      , "COD","DJI","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN"
      , "GNB","CIV","KEN","LSO","LBR","MDG","MWI","MLI","MRT","MUS"
      , "MOZ","NAM","NER","NGA","COG","RWA","STP","SEN","SYC","SLE"
      , "SOM","ZAF","SSD","TZA","TGO","UGA","ZMB","ZWE"
      ]

  , mk "ANZ"
      ["AUS","NZL"]

  , mk "GULF"
      ["BHR","IRN","IRQ","KWT","OMN","QAT","SAU","ARE","YEM"]

  ]
  where
    mk :: Text -> [Text] -> (RegionId, [CountryId])
    mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)
        -- mk :: Text -> [Text] -> [(RegionId, CountryId)]
    -- mk r = map (\c -> (RegionId r, CountryId c))

 
regionNames :: [Region]
regionNames =
  [ Region (RegionId "SAMERICA")      "Südamerika"
  , Region (RegionId "USCAN")         "USA & Kanada"
  , Region (RegionId "EUROPE")        "Europa"
  , Region (RegionId "RUSSIA")        "Russland"
  , Region (RegionId "CHINA")         "China"
  , Region (RegionId "INDIA")         "Indien"
  , Region (RegionId "JAPAN")         "Japan"
  , Region (RegionId "FAREAST")       "Fernost"
  , Region (RegionId "CENTRAL_ASIA")  "Zentralasien"
  , Region (RegionId "SOUTH_ASIA")    "Südasien"
  , Region (RegionId "NORTH_AFRICA")  "Nordafrika"
  , Region (RegionId "SUBSAHARA")     "Subsahara-Afrika"
  , Region (RegionId "ANZ")           "Australien & Neuseeland"
  , Region (RegionId "GULF")          "Golfstaaten"
  ]
 



