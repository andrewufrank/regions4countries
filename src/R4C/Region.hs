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
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
import R4C.Indicator

regionMembers :: [(RegionId, CountryId)]
regionMembers = concat
  [ mk "USCAN"
      ["CAN","USA"]

  , mk "SAMERICA"
      [ "ATG","ARG","ABW","BHS","BRB","BLZ","BOL","BRA","CHL","COL"
      , "CRI","CUB","DMA","DOM","ECU","SLV","GRD","GTM","GUY","HTI"
      , "HND","JAM","MEX","NIC","PAN","PRY","PER","KNA","LCA","VCT"
      , "SUR","TTO","URY","VEN"
      ]

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
    -- mk :: Text -> [Text] -> [(RegionId, CountryId)]
    mk r = map (\c -> (RegionId r, CountryId c))

countriesInRegion :: RegionId -> [CountryId]
countriesInRegion r =
  [ c | (r', c) <- regionMembers, r' == r ]

debugRegion :: Connection -> IndicatorId -> Year -> RegionId -> IO ()
debugRegion conn ind yr reg = do
  rows <- query conn
    "SELECT cr.country, o.value \
    \FROM country_region cr \
    \LEFT JOIN observation o \
    \  ON o.country = cr.country \
    \ AND o.indicator = ? \
    \ AND o.year = ? \
    \WHERE cr.region = ? \
    \ORDER BY cr.country"
    (ind, yr, reg)
      :: IO [(CountryId, Maybe Value)]

  mapM_ print rows

testdr = do   
    conn <- open "test.sqlite"
    debugRegion conn popid (Year 2024) (RegionId "USCAN")
    debugRegion conn popid (Year 2024) (RegionId "EUROPE")
    debugRegion conn popid (Year 2024) (RegionId "SAMERICA")
    close conn

  where 
        popid = indicatorId population -- IndicatorId "SP.POP.TOTL"

testRegions = do
  conn <- open "test.sqlite"

  rows <- query_ conn
    "SELECT region, country FROM country_region ORDER BY region, country LIMIT 50"
      :: IO [(RegionId, CountryId)]

  mapM_ print rows
  close conn

{-    
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
       map (RegionId "G7",CountryId) ["CAN","FRA", "DEU", "ITA", "JPN", "GBR", "USA" ])
  ]
-}

