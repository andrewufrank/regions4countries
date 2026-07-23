-----------------------------------------------------------------------------
--
-- Module      :  remove the small (less1m) countries from the regions 
-- especially small countries 

-----------------------------------------------------------------------------

module R4C.Region3
    where

import R4C.Model
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator 
import Study.Region2 
import R4C.Aggregate 
import R4C.Import.Query
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 
import Study.Dataset 
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UniformBase hiding ((</>))
import R4C.Import.Database
-- import R4C.Export.CountryTable
import R4C.Export.Table 
import R4C.Export.CountnryCodeNames
import qualified Data.Set as Set
import R4C.CountryExperiments

import Data.List (intercalate)
import Data.Text (unpack)

less1M :: Set.Set CountryId
less1M = Set.fromList $ map CountryId less1Mcountries 

removeSmallCountries :: RegionMembers -> RegionMembers
removeSmallCountries =
    map $ \(rid, cs) ->
        (rid, filter (`Set.notMember` less1M) cs)

regionMembers3 = removeSmallCountries regionMembers2

allRegionCountries :: [CountryId]
-- | all countries which are in any region
allRegionCountries =
    concatMap snd regionMembers3

missingCountryIds :: [Text]
-- | the ones missing 
missingCountryIds =
    filter (`notElem` allRegionCountriesList) (  countriesOnlyList)



dumpRegionMembers :: RegionMembers -> IO ()
dumpRegionMembers rms = do
    putStrLn "regionMembers3 :: RegionMembers"
    putStrLn "regionMembers3 ="
    putStrLn "  ["
    mapM_ dumpRegion rms
    putStrLn "  ]"
  where
    dumpRegion (rid, cs) = do
        putStrLn $ "    , mk " ++ showRegion rid
        putStrLn $ "      [ " ++ intercalate "," (map showCountry cs) ++ " ]"

    showRegion (RegionId r) = show (unpack r)
    showCountry (CountryId c) = show (unpack c)

regionMembers3pp :: RegionMembers
-- the regionMembers3 prettyprinted to study 
regionMembers3pp =
  [
     mk "SAMERICA"
      [ "ARG","BOL","BRA","CHL","COL","CRI","CUB","DOM","ECU","SLV","GTM","HTI","HND",
        "JAM","MEX","NIC","PAN","PRY","PER","TTO","URY","VEN" ]
    , mk "USCAN"
      [ "USA","CAN" ]
    , mk "EUROPE"
      [ "ALB","AUT","BLR","BEL","BIH","BGR","HRV","CYP","CZE","DNK","EST",
        "FIN","FRA","DEU","GRC","HUN","IRL","ITA","XKX","LVA","LTU","MDA",
        "NLD","MKD","NOR","POL","PRT","ROU","SRB","SVK","SVN","ESP",
        "SWE","CHE","TUR","UKR","GBR","VAT" ]
    , mk "RUSSIA"
      [ "RUS" ]
    , mk "CHINA"
      [ "CHN" ]
    , mk "INDIA"
      [ "IND" ]
    , mk "JAPAN"
      [ "JPN" ]
    , mk "FAREAST"
      [ "KHM","IDN","LAO","MYS","MNG","MMR","PRK","PHL","SGP",
        "KOR","TWN","THA","VNM" ]
    , mk "CENTRAL_ASIA"
      [ "UZB","KAZ","TJK","KGZ","TKM" ]
    , mk "SOUTH_ASIA"
      [ "PAK","BGD","AFG","NPL","LKA" ]
    , mk "NORTH_AFRICA"
      [ "DZA","EGY","LBY","MAR","SDN","TUN","ESH" ]
    , mk "SUBSAHARA"
      [ "AGO","BEN","BWA","BFA","BDI","CMR","CAF","TCD","COD",
        "DJI","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN","GNB",
        "CIV","KEN","LSO","LBR","MDG","MWI","MLI","MRT","MUS","MOZ",
        "NAM","NER","NGA","COG","RWA","SEN","SLE","SOM","ZAF",
        "SSD","TZA","TGO","UGA","ZMB","ZWE" ]
    , mk "ANZ"
      [ "AUS","NZL" ]
    , mk "GULF"
      [ "BHR","IRN","IRQ","KWT","OMN","QAT","SAU","ARE","YEM" ]
  ]
    where
    mk :: Text -> [Text] -> (RegionId, [CountryId])
    mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)

allRegionCountriesList :: [Text]
allRegionCountriesList =  -- all countries which are in any region 
    ["ARG","BOL","BRA","CHL","COL","CRI","CUB","DOM","ECU","SLV","GTM","HTI","HND","JAM","MEX","NIC","PAN","PRY","PER","TTO","URY","VEN","USA","CAN","ALB","AUT","BLR","BEL","BIH","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","XKX","LVA","LTU","MDA","NLD","MKD","NOR","POL","PRT","ROU","SRB","SVK","SVN","ESP","SWE","CHE","TUR","UKR","GBR","VAT","RUS","CHN","IND","JPN","KHM","IDN","LAO","MYS","MNG","MMR","PRK","PHL","SGP","KOR","TWN","THA","VNM","UZB","KAZ","TJK","KGZ","TKM","PAK","BGD","AFG","NPL","LKA","DZA","EGY","LBY","MAR","SDN","TUN","ESH","AGO","BEN","BWA","BFA","BDI","CMR","CAF","TCD","COD","DJI","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN","GNB","CIV","KEN","LSO","LBR","MDG","MWI","MLI","MRT","MUS","MOZ","NAM","NER","NGA","COG","RWA","SEN","SLE","SOM","ZAF","SSD","TZA","TGO","UGA","ZMB","ZWE","AUS","NZL","BHR","IRN","IRQ","KWT","OMN","QAT","SAU","ARE","YEM"]

countriesInNoRegionList :: [Text] 
countriesInNoRegionList = 
    ["ASM","AND","ATG","ARM","ABW","AZE","BHS","BRB","BLZ","BMU","BTN","VGB","BRN","CPV","CYM","CHI","COM","CUW","DMA","FRO","FJI","PYF","GEO","GIB","GRL","GRD","GUM","GUY","HKG","ISL","IMN","ISR","JOR","KIR","LBN","LIE","LUX","MAC","MDV","MLT","MHL","FSM","MCO","MNE","NRU","NCL","MNP","PLW","PNG","PRI","WSM","SMR","SYC","SXM","SLB","KNA","LCA","MAF","VCT","SUR","SYR","STP","TLS","TON","TCA","TUV","VUT","VIR","PSE"]

-- 

extraRegions3  :: RegionMembers   
extraRegions3 =  
  [ mk "notInRegion" countriesInNoRegionList
  , mk "World Totals" ["WLD"]
  , mk "G7"
        ["CAN","FRA","DEU","ITA","JPN","GBR","USA"]

  , mk "EU"
        ["AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA"
        ,"DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD"
        ,"POL","PRT","ROU","SVK","SVN","ESP","SWE"]
  , mk "BRICS"
    [ "BRA","RUS","IND","CHN","ZAF"
    , "EGY","ETH","IDN","IRN","SAU","ARE"
    ]

    , mk "OPEC"
    [ "DZA","COG","GNQ","GAB","IRN","IRQ","KWT"
    , "LBY","NGA","SAU","ARE","VEN"
    ]

    , mk "SCO"
    [ "BLR","CHN","IND","IRN","KAZ","KGZ"
    , "PAK","RUS","TJK","UZB"
    ]
  ]
    where
    mk :: Text -> [Text] -> (RegionId, [CountryId])
    mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)
