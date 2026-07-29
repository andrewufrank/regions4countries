-----------------------------------------------------------------------------
--
-- Module      :   experiments with Terry data  for counries 
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
import Study.Descriptor
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UniformBase hiding ((</>))
import R4C.Import.Database
-- import R4C.Export.CountryTable
import R4C.Export.Table 
import R4C.Export.CountnryCodeNames
import qualified R4C.Region3 as R3 
import qualified BaseTest.Region as RBT 
import BaseTest.Region (regionOrder)
import Data.List
import R4C.Territory





--     values =  map tvValue  table 



-- lookupRegionTable2 :: Connection -> [(RegionId, [CountryId])] -> IO [(RegionId,  [CountryTable])]
lookupRegionTable2 :: Connection -> [(RegionId, [CountryId])] -> Dataset -> Year -> IO [(RegionId, CountryTable)]
-- fill for each region a countryTable with only its countries 
lookupRegionTable2 conn regionDef ds yr = do 
        worldTab <- lookupTable conn (dsIndicator ds) yr  
        let regTab = map (\(reg, cts) -> (reg, countryTable worldTab cts)) regionDef
        return regTab 




-- lookupCountryTable :: Connection -> (Dataset, Year) -> IO CountryTable
lookupCountryTable :: Connection -> (Dataset, Year) -> IO (MdColumn CountryId Double)
lookupCountryTable conn (d, y) = do
    tab <-  lookupTable conn (dsIndicator d) y
    return $ wrapMdCol d tab



exp4:: IO ()  
-- | show only one indicator 
exp4 = do
    let
        regionDef = RBT.regionMembers -- g7, eu, russia 

    conn <- open dbPath 
    regionCountryTable :: RegionTable3  <- lookupRegionTable3 conn regionDef ( population) (Year 2024)
    close conn

    -- let aggs = map (\op -> aggregateTerry3 op regionCountryTable) [min1, median1, max1, mean1, stdDev1] 
    -- putIOwords ["the sums are", showT aggs]
    
    -- convert to regionTable 
    -- make a single val for each country 
    let rct = snd regionCountryTable :: [(RegionId, CountryTable)]
        rct2  :: [TerryValue RegionId Double ]
        rct2 =  sumCountryTables rct 

    -- let  mdC = map (\(t,d) -> wrapMdCol2 d t) $ zip rcTab2rTab req
    let  mdC = wrapMdCol ( population) rct2  
-- the operations on the tables must be with the mdcol data! 

    let md = markdownTable RBT.regionNames regionOrder [mdC]

    putStrLn md 
    --extract one region and show the country data 
    let regid = RegionId "EU"
    let euMdC = getOneRegionMany3  [regionCountryTable] regid::   [MdColumn CountryId Double]
  
    let md =  (markdownTable allCodeNames euCountries)    euMdC--less1m mdC
    putStrLn  md 

    return ()

-- the operations on the tables must be with the mdcol data! 
            -- let sortedRegions = sortTerryByColumn Descending  ctTab  
            -- let md = markdownTable allCodeNames sortedRegions [mdC]  --less1m mdC
            -- putStrLn md 



euCountries = map CountryId ["AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA"
        ,"DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD"
        ,"POL","PRT","ROU","SVK","SVN","ESP","SWE"]


exp3 = do 
    _ <- exp3a regionOrder   threeCountries  
    return ()
-- exp3 :: IO ()  
-- | show all countries with popuplation surface and GNP
exp3a regOrder countriesOrder = do
    let
        countries = less1m  -- countries included 
        regionDef = RBT.regionMembers -- g7, eu, russia 

    conn <- open dbPath 
    let req = [population, gnpPP, surfaceArea]
        years = map Year [2024, 2024, 2023]
        reqYears = zip ( req) years-- :: [(Dataset, Year)]
    regionCountryTables :: [RegionTable3]  <- mapM (\(d,y) -> lookupRegionTable3 conn regionDef d y) reqYears
    close conn

    let aggs = map (\a -> map (aggregateTerry3 a ) regionCountryTables) [min1, median1, max1, mean1, stdDev1] 
    putIOwords ["the sums are", showT aggs]
    
    -- convert to regionTable 
    -- make a single val for each country 
    let rct = regionCountryTables :: [RegionTable3] -- (Dataset, [(RegionId, CountryTable)]) 
        rct2  :: [(Dataset, [TerryValue RegionId Double ])]
        rct2 = map xone rct
        xone :: (Dataset, [(RegionId, CountryTable)]) -> (Dataset, [TerryValue RegionId Double ]) 
        xone (ds, tab) = (ds,  sumCountryTables tab)


    let  mdC = wrapMdCol3 rct2
    -- let  mdC = map (\(t,d) -> wrapMdCol d t) t2 req
-- the operations on the tables must be with the mdcol data! 

    let md1 = markdownTable RBT.regionNames regOrder mdC
    putStrLn md1 

    -- get OneCountry 
    let regid = RegionId "EU"
    -- let euMdC = catMaybes $ zipWith (\pop tab -> getOneRegion regid pop  tab) req rct  :: [MdColumn CountryId Double]
    let euMdC = getOneRegionMany3  rct regid
    let md2 =  (markdownTable allCodeNames countriesOrder) $   euMdC --less1m mdC
    putStrLn  md2 
    return (md1, md2)





less1m = map CountryId R3.less1mTax
-- break was 60k$ GNP 2024 and less 1 mio P 


-- exp1a :: IO ()  
-- | show all countries with popuplation surface and GNP
-- fig11 
exp1a countries= do
    -- let countries = less1m  -- countries included 
    conn <- open dbPath 
    let req = [population, gnpPP, surfaceArea]
        years = map Year [2024, 2024, 2023]
        reqYears = zip (map dsIndicator req) years-- :: [(Dataset, Year)]
    countryTables :: [TerryTable CountryId ( Double)] <- mapM (\(d,y) -> lookupTable conn ( d) y) reqYears
    close conn

    let aggs = map (\a -> map (aggregateTery a countries) countryTables) [sum1, min1, median1, max1, mean1, stdDev1] 

    putIOwords ["the sums are", showT aggs]

    let  mdC = map (\(t,d) -> wrapMdCol d t) $ zip countryTables req :: [MdColumn CountryId Double]
-- the operations on the tables must be with the mdcol data! 
    -- let sortedRegions = sortTerryByColumn Descending  (headNote "wewer" countryTables) 
    let md = markdownTable allCodeNames countries mdC  --less1m mdC
    putStrLn md 
    -- putStrLn . show . map unCountryId $ less1m
    -- writeTab1Table "exp1" md
    -- putStrLn . show $ sortedRegions
    return (md)

threeCountries = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
exp1 = do 
    _ <- exp1a less1m 
    return ()
exp1t = exp1a threeCountries

-- exp2 :: IO ()
-- -- } show the totals for the small, and very small countries
-- exp2 = do 
--     conn <- open dbPath 


--     tabPop :: CountryTable  <- lookupTable conn (dsIndicator population)(Year 2024)
--     tabGNP :: CountryTable <- lookupTable conn (dsIndicator grossNatProd) (Year 2024)
--     tabSurf :: CountryTable <- lookupTable conn (dsIndicator surfaceArea) (Year 2023)
--     -- for weighted values aggregateTable does not work
--     -- tabGNPpcwb <- lookupTable conn (dsIndicator gnpPPpc) (Year 2024)
--     -- gnpPCwb <-  (aggregate regionMembersSmall conn gnpPPpc (Year 2024))  

--     let 
--         pops = aggregateTable (aggregationFunction $ dsAggregation population) regionMembersSmall  tabPop
--         surfs = aggregateTable (aggregationFunction $ dsAggregation surfaceArea) regionMembersSmall tabSurf 
--         gnp = aggregateTable (aggregationFunction $ dsAggregation grossNatProd) regionMembersSmall tabGNP
--         -- gnpPCwb =  aggregateTable (aggregationFunction $ dsAggregation gnpPPpc) regionMembersSmall tabGNPpcwb

--         gnpPC = combineRegionTables Divide gnp pops
    
--     close conn

--     let mdCols = 
--             [ pops, surfs, gnp, gnpPC
--             -- , gnpcWB 
            
--             -- MdColumn "Bevoelkerung 2024 (Mega)" Mega 0  pops
--             -- , MdColumn "Flaeche 2023 (Mega km²)" Mega 2  surfs
--             -- , MdColumn "GNP (Mega $)" Giga 0 gnp
--             -- , MdColumn "GNP pc (Kilo $)" Kilo 0 gnpPC  
--             -- , MdColumn "GNP pc WorldBank cal (Kilo $)" Kilo 0 gnpPCwb 
--             --         Centi 1 surfPerCap
--             -- , MdColumn "Nutzbares Land per capita (ha/person)" 
--             --         Centi 0 surAgrarfPerCap 
--             ]
--     let sortedRegions = sortTerryByColumn Descending  pops  

--     -- let md = markdownTable regionsList mdCols
--     let md = markdownTable regionNames2 sortedRegions mdCols
--     putStrLn md 
--     writeTab1Table "tab11" md


-- regionMembersSmall :: RegionMembers  -- [(RegionId, CountryId)]
-- regionMembersSmall = 
--   [ mk "less10mio" less10Mcountries 
--     , mk "less1mio" less1Mcountries
--   ]
--   where
--     mk :: Text -> [Text] -> (RegionId, [CountryId])
--     mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)

-- tableOutputDirectoryExp :: FilePath
-- tableOutputDirectoryExp =   "/home/frank/CountryExperiments"

-- writeTab1Table :: FilePath -> String -> IO ()
-- writeTab1Table filename contents = do
--     createDirectoryIfMissing True tableOutputDirectoryExp
--     writeFile (  tableOutputDirectoryExp </> filename) contents
