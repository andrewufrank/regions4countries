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
-- import R4C.Aggregate 
import R4C.Import.Query
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 
import           Study.Descriptor
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UniformBase hiding ((</>))
import R4C.Import.Database
-- import R4C.Export.CountryTable
-- import R4C.Export.Table 
import R4C.Export.CountnryCodeNames
import qualified Data.Set as Set
-- import R4C.CountryExperiments

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


less1mTax = ["LUX","MAC","BMU","CYM","BRN","ISL","FRO","GUY","AND","MLT"] :: [Text]

less1mNonTax = ["SXM","ABW","BHS","TCA","KNA","MNE","ATG","SYC","CUW","LCA","MDV","BRB","SUR","DMA","VCT","PLW","GRD","BTN","FJI","BLZ","NRU","CPV","WSM","MHL","TON","TUV","STP","FSM","VUT","COM","KIR","SLB"]

countriesOnlyList :: [Text]
countriesOnlyList = ["AFG","ALB","DZA","ASM","AND","AGO","ATG","ARG","ARM","ABW","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BMU","BTN","BOL","BIH","BWA","BRA","VGB","BRN","BGR","BFA","BDI","CPV","KHM","CMR","CAN","CYM","CAF","TCD","CHI","CHL","CHN","COL","COM","COD","COG","CRI","HRV","CUB","CUW","CYP","CZE","CIV","DNK","DJI","DMA","DOM","ECU","EGY","SLV","GNQ","ERI","EST","SWZ","ETH","FRO","FJI","FIN","FRA","PYF","GAB","GMB","GEO","DEU","GHA","GIB","GRC","GRL","GRD","GUM","GTM","GIN","GNB","GUY","HTI","HND","HKG","HUN","ISL","IND","IDN","IRN","IRQ","IRL","IMN","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KIR","PRK","KOR","XKX","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LIE","LTU","LUX","MAC","MDG","MWI","MYS","MDV","MLI","MLT","MHL","MRT","MUS","MEX","FSM","MDA","MCO","MNG","MNE","MAR","MOZ","MMR","NAM","NRU","NPL","NLD","NCL","NZL","NIC","NER","NGA","MKD","MNP","NOR","OMN","PAK","PLW","PAN","PNG","PRY","PER","PHL","POL","PRT","PRI","QAT","ROU","RUS","RWA","WSM","SMR","SAU","SEN","SRB","SYC","SLE","SGP","SXM","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","KNA","LCA","MAF","VCT","SDN","SUR","SWE","CHE","SYR","STP","TJK","TZA","THA","TLS","TGO","TON","TTO","TUN","TKM","TCA","TUV","TUR","UGA","UKR","ARE","GBR","USA","URY","UZB","VUT","VEN","VNM","VIR","PSE","YEM","ZMB","ZWE"]

less10Mcountries :: [Text]
less10Mcountries = ["ABW","ALB","AND","ARM","ASM","ATG","AUT",
    "BGR","BHR","BHS","BIH","BLR","BLZ","BMU","BRB","BRN","BTN",
    "BWA","CAF","CHE","CHI","COG","COM","CPV","CRI","CSS","CUW","CYM","CYP",
    "DJI","DMA","DNK","ERI","EST","FIN","FJI","FRO","FSM",
    "GAB","GEO","GIB","GMB","GNB","GNQ","GRD","GRL","GUM","GUY",
    "HKG","HRV","HUN","IMN","IRL","ISL","JAM","KGZ","KIR","KNA","KWT",
    "LAO","LBN","LBR","LBY","LCA","LIE","LSO","LTU","LUX","LVA",
    "MAC","MAF","MCO","MDA","MDV","MHL","MKD","MLT","MNE","MNG","MNP","MRT","MUS",
    "NAM","NCL","NIC","NOR","NRU","NZL","OMN","PAN","PLW","PRI","PRY","PSE","PSS","PYF",
    "QAT","SGP","SLB","SLE","SLV","SMR","SRB","STP","SUR","SVK","SVN","SWZ","SXM","SYC",
    "TCA","TGO","TKM","TLS","TON","TTO","TUV","URY","VCT","VGB","VIR","VUT","WSM","XKX"]

less1Mcountries :: [Text]
less1Mcountries = ["ABW","AND","ASM","ATG","BHS","BLZ","BMU","BRB","BRN","BTN",
    "CHI","COM","CPV","CUW","CYM","DMA","FJI","FRO","FSM","GIB","GRD","GRL","GUM","GUY",
    "IMN","ISL","KIR","KNA","LCA","LIE","LUX","MAC","MAF","MCO",
    "MDV","MHL","MLT","MNE","MNP","NCL","NRU","PLW","PYF",
    "SLB","SMR","STP","SUR","SXM","SYC","TCA","TON",
    "TUV","VCT","VGB","VIR","VUT","WSM"]


regionOrder = [
    RegionId "USCAN",
    RegionId "SAMERICA",
    RegionId "EUROPE",
    RegionId "NORTH_AFRICA",
    RegionId "SUBSAHARA",
    RegionId "RUSSIA",
    RegionId "CENTRAL_ASIA",
    RegionId "GULF",
    RegionId "INDIA",
    RegionId "SOUTH_ASIA",
    RegionId "CHINA",
    RegionId "FAREAST",
    RegionId "JAPAN",
    RegionId "ANZ",

    RegionId "notInRegion",

    RegionId "World Totals",
    RegionId "EU",
    RegionId "G7",
    RegionId "OPEC",
    RegionId "BRICS",

    RegionId "SCO"]