-----------------------------------------------------------------------------
--
-- Module      :   experiments with Terry data 
-- especially small countries 

-----------------------------------------------------------------------------

module R4C.CountryExperiments
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

less1Mcountries = ["ABW","AND","ASM","ATG","BHS","BLZ","BMU","BRB","BRN","BTN",
    "CHI","COM","CPV","CUW","CYM","DMA","FJI","FRO","FSM","GIB","GRD","GRL","GUM","GUY",
    "IMN","ISL","KIR","KNA","LCA","LIE","LUX","MAC","MAF","MCO",
    "MDV","MHL","MLT","MNE","MNP","NCL","NRU","PLW","PYF",
    "SLB","SMR","STP","SUR","SXM","SYC","TCA","TON",
    "TUV","VCT","VGB","VIR","VUT","WSM"]

regionMembersSmall :: RegionMembers  -- [(RegionId, CountryId)]
regionMembersSmall = 
  [ mk "less10mio" less10Mcountries 
    , mk "less1mio" less1Mcountries
  ]
  where
    mk :: Text -> [Text] -> (RegionId, [CountryId])
    mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)

tableOutputDirectory :: FilePath
tableOutputDirectory =   "/home/frank/CountryExperiments"

writeTab1Table :: FilePath -> String -> IO ()
writeTab1Table filename contents = do
    createDirectoryIfMissing True tableOutputDirectory
    writeFile (  tableOutputDirectory </> filename) contents



exp2 :: IO ()
-- } show the totals for the small, and very small countries
exp2 = do 
    conn <- open dbPath 
    pops <-  (aggregate regionMembersSmall conn population (Year 2024)) 
    surfs <-  (aggregate regionMembersSmall conn surfaceArea (Year 2023)) 
    gnp <-  (aggregate regionMembersSmall conn grossNatProd (Year 2024))  
    gnpPCwb <-  (aggregate regionMembersSmall conn gnpPPpc (Year 2024))  
    -- fertility <-  (aggregate regionMembersSmall conn fertilityRate (Year 2024))  
    -- agrar <-  (aggregate regionMembersSmall conn agrarland (Year 2024))  

    let gnpPC = combineRegionTables (/) gnp surfs
    -- let surAgrarfPerCap = combineRegionTables (/) agrar pops3
    -- let netmigPC = combineRegionTables (/) netmigration pops3 
    
    close conn

    let mdCols = 
            [ MdColumn "Bevoelkerung 2024 (Mega)" Mega 0  pops
            , MdColumn "Flaeche 2023 (Mega km²)" Mega 2  surfs
            , MdColumn "GNP (Mega $)" Giga 0 gnp
            , MdColumn "GNP pc (Kilo $)" Kilo 0 gnpPC  
            , MdColumn "GNP pc WorldBank cal (Kilo $)" Kilo 0 gnpPCwb 
            --         Centi 1 surfPerCap
            -- , MdColumn "Nutzbares Land per capita (ha/person)" 
            --         Centi 0 surAgrarfPerCap 
            ]
    let sortedRegions = sortTerryByColumn Descending  pops --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 sortedRegions mdCols
    putStrLn md 
    writeTab1Table "tab11" md


exp1 :: IO ()  
-- | show all countries with popuplation surface and GNP
-- fig11 
exp1 = do
    conn <- open dbPath 
    tabPop :: CountryTable  <- lookupTable conn (dsIndicator population)(Year 2024)
    tabGNPpc :: CountryTable <- lookupTable conn (dsIndicator gnpPPpc) (Year 2024)
    tabSurf <- lookupTable conn (dsIndicator surfaceArea) (Year 2023)

    close conn

    let mdCols = 
            [ MdColumn "Bevoelkerung 2024 (Mega)" Mega 6  tabPop
            , MdColumn "Flaeche 2023 (Kilo km²)" Kilo 2  tabSurf
            , MdColumn "GNP pro Kopf (kilo PP)" Kilo 3 tabGNPpc 
             
            ]
    let tabPop' = sortTerryByColumn Descending  tabPop --surfPerCap

    let md = markdownTable allCodeNames tabPop' mdCols
    putStrLn md 
    writeTab1Table "exp1" md
    return ()

