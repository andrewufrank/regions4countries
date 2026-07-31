-- {-# ANN myFunction ("HLint: ignore Use fewer guards" :: String) #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-----------------------------------------------------------------------------
--
-- Module      :   experiments with Terry data  for counries 
-- especially small countries 

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BaseTest.CountryExperiments
    where

import R4C.Model
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator 
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
import R4C.Territory
import R4C.Pak 


dreiEu = map CountryId ["FIN", "CYP", "PRT"]

exp7 = do   -- get regions from exp1a
    _ <- exp7a euCountries RBT.regionMembers  -- EU, G7
    return ()

-- exp7 :: [CountryId] -> IO String
-- construct the country tables as Pak 
-- countries is what is included in printed list
-- combine extensive datasets then construct region and combine more 

exp7a :: [CountryId] -> [(RegionId, [CountryId])] -> IO (String, String)
exp7a countries regionDef = do
    conn <- open dbPath 
    let  reqYears =  [(population, Year 2024), (surfaceArea, Year 2023), (gdpPPpc, Year 2021)]

    c3@[pop3, surf3, gdp3] :: [CountryTable3] <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
    close conn

    let gpc4 :: CountryTable3 
        gpc4 = (ds1, combineTerryTables (*) (snd gdp3) (snd pop3))
        ds1 = Dataset{dsShortName ="gnp"
                    , dsScale = Giga
                    , dsUnit = "PP$"
                    , dsDecimals = 0 
                    , dsExtensive = True}        
        c4 = c3 ++ [gpc4]  -- the countries with difference between GDP and GNP (cyprus, ireland, luxemburg, malta)
-- the operations on the tables must be with the mdcol data!
        mc4 = wrapMdCol3 c4
    let md = markdownTable allCodeNames countries mc4  --less1m mdC
    putStrLn md 

    let reg4 = reg3CountryTable4 regionDef c4 :: [(Dataset, [(RegionId, TerryTable CountryId Double)])]
        reg4tot@[pop4, surf4, _, gnp4] =  regtab3_regtab1 reg4 :: [(Dataset, [TerryValue RegionId Double])]
        -- gnpPC4 = (ds2, combineTerryTables (/) (snd gnp4) (snd pop4))
        gnpPC4 = combinesCountryTable3 ds2 (/)  gnp4 pop4
        ds2 = Dataset{dsExtensive = False
            , dsShortName ="gnp per cap."
            , dsScale = Kilo
            , dsUnit = "$/P"
            , dsDecimals = 0 }
    -- to test comp gdp per cap aus gpc4 and pop3
        md4 = wrapMdCol3 (reg4tot ++ [gnpPC4]) :: [MdColumn RegionId Double]
    let md2 = markdownTable RBT.regionNames RBT.regionOrder md4  --less1m mdC
    putStrLn md2 
    -- print md2
    return (md, md2)



                    


-- -- produce country tables then extract the extensionl columns to region
-- difficult to combine region tables 
-- exp6 = do 
--     _ <- exp6b regionOrder dreiEu   -- european!
--     return ()
-- -- exp3 :: IO ()  
-- -- | show all countries with popuplation surface and GNP

-- exp6b :: [RegionId] -> [CountryId] -> IO String
-- exp6b regOrder countries = do
--     let regionDef = RBT.regionMembers -- g7, eu, russia 

--     conn <- open dbPath 
--     let reqYears = [(population, Year 2024), (surfaceArea, Year 2023),  (gdpPPpc, Year 2021)]
--     [pop3, surf3, gdp3] :: [RegionTable3]  <- mapM (\(d,y) -> lookupRegionTable3 conn regionDef d y) reqYears

--     close conn

--     -- combineMdTables 
--     let 
--         gd4 = (ds1, combineTerryTables (/) (snd pop3) (snd gdp3)) 
--         ds1 = Dataset{dsName = "gdpPP"
--                     , dsShortName ="gdpPP"
--                     , dsScale = Giga
--                     , dsUnit = "PP$"
--                     , dsDecimals = 0 }
--     -- let [gnp3, surf3] = map wrapMdCol3 [reg3gnp, reg3surf] 

--     let 
--         reg3s = [pop3, surf3, gdp3] ++ [gn3, surf3] :: [RegionTable3]
--     let mdC :: [MdColumn RegionId Double]
--         mdC = wrapMdCol3 . regtab3_regtab1 $ reg3s
--     let md1 = markdownTable RBT.regionNames regOrder mdC
--     putStrLn md1 

--     -- get OneCountry 
--     let regid = RegionId "EU"
--     let euMdC = getOneRegionMany3  reg3s regid -- implies the wrap
--     -- let md3 = wrapMdCol3 euMdC :: [MdColumn CountryId Double]
--     let md2 =  markdownTable allCodeNames countries    euMdC --less1m mdC
--     putStrLn  md2 
--     -- print md1 
--     -- print md2
--     return md2


-- -- produce country tables then extract the extensionl columns to region
-- exp3 = do 
--     _ <- exp3b regionOrder dreiEu   -- european!
--     return ()
-- -- exp3 :: IO ()  
-- -- | show all countries with popuplation surface and GNP
-- exp3b :: [RegionId] -> [CountryId] -> IO String
-- exp3b regOrder countries = do
--     let regionDef = RBT.regionMembers -- g7, eu, russia 

--     conn <- open dbPath 
--     let reqYears = [(population, Year 2024),  (gdpPPpc, Year 2021)]
--     [pop3,gdp3] :: [RegionTable3]  <- mapM (\(d,y) -> lookupRegionTable3 conn regionDef d y) reqYears
--     let req2 = [(gnp, Year 2021), (surfaceArea, Year 2023)]

--     [gn3, surf3] :: [RegionTable3] <- mapM (\(d,y) -> lookupRegionTable3 conn regionDef d y) req2 

--     close conn

--     -- combineMdTables -- difficult for region tables 
 
--     -- let [gnp3, surf3] = map wrapMdCol3 [reg3gnp, reg3surf] 

--     let reg3s = [pop3,gdp3] ++ [gn3, surf3] :: [RegionTable3]
--     let mdC :: [MdColumn RegionId Double]
--         mdC = wrapMdCol3 . regtab3_regtab1 $ reg3s
--     let md1 = markdownTable RBT.regionNames regOrder mdC
--     putStrLn md1 

--     -- get OneCountry 
--     let regid = RegionId "EU"
--     let euMdC = getOneRegionMany3  reg3s regid -- implies the wrap
--     -- let md3 = wrapMdCol3 euMdC :: [MdColumn CountryId Double]
--     let md2 =  markdownTable allCodeNames countries    euMdC --less1m mdC
--     putStrLn  md2 
--     -- print md1 
--     -- print md2
--     return md2


-- exp5 = do   -- get regions from exp1a
--     _ <- exp5a euCountries RBT.regionMembers  -- EU, G7
--     return ()

-- -- exp5 :: [CountryId] -> IO String
-- -- countries is what is included in printed list 
-- exp5a :: [CountryId] -> [(RegionId, [CountryId])] -> IO String
-- exp5a countries regionDef = do
--     conn <- open dbPath 
--     let  reqYears =  [(population, Year 2024), (gnpPP, Year 2021),  (gdpPPpc, Year 2021)]

--     countryTables :: [CountryTable3] <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
--     close conn
--     let mdC = wrapMdCol3 countryTables
--     let [pop3, g3, gpc3] = mdC :: [MdColumn CountryId Double]

--     let gpc4 = (combineMdTables Divide g3  pop3){colScale=Kilo}
--         diff4 = (combineMdTables (Subtract) gpc3 gpc4)
--         mdC4 = mdC ++ [gpc4, diff4]  -- the countries with difference between GDP and GNP (cyprus, ireland, luxemburg, malta)
-- -- the operations on the tables must be with the mdcol data! 
--     let md = markdownTable allCodeNames countries mdC4  --less1m mdC
--     putStrLn md 

--     -- now for the regions
--     let [popt, gt, gpct]= countryTables
--         gt:: (Dataset, TerryTable CountryId Double) 
--         gnct = (ds1, combineTerryTables (/) (snd gt) (snd popt)) 
--         difft = (ds2, combineTerryTables (-) (snd gnct) (snd gpct))
--         ds1 = Dataset{dsName = "gnpPP per cap"
--                     , dsShortName ="gnpPP per cap"
--                     , dsScale = Kilo
--                     , dsUnit = "PP$/P"
--                     , dsDecimals = 0 }
--         ds2 = Dataset{dsName = "diff"
--                     , dsShortName ="diff"
--                     , dsScale = Kilo
--                     , dsUnit = "PP$/P"
--                     , dsDecimals = 0 }        -- gpc4 = MdColumn { colValues = combineTerryTables (/) (snd gt) (snd popt) 
--         --                 , colTitle = "gnpPP per cap"
--         --                 , colScale = Kilo
--         --                 , colUnit = "PP$"
--         --                 , colDecimals = 0
--         --                 }   

--         -- gt4 = [pop3, g3, gpc3] ++ [gpc4]
--         -- -- reg3CountryTable4 regionDef mdC
--         gt4 = countryTables ++ [gnct, difft] :: [CountryTable3]
--         reg4:: [(Dataset, [(RegionId, TerryTable CountryId Double)])]
--         reg4 = reg3CountryTable4 regionDef gt4 :: [(Dataset, [(RegionId, TerryTable CountryId Double)])]
--         reg4tot =  regtab3_regtab1 reg4 :: [(Dataset, [TerryValue RegionId Double])]
--         -- reg4tot = map (second sumCountryTables) reg4 
--         md4 = wrapMdCol3 reg4tot :: [MdColumn RegionId Double]
--     let md2 = markdownTable RBT.regionNames RBT.regionOrder md4  --less1m mdC
--     putStrLn md2 

--     print md
--     return (md)


 


-- exp1 = do 
--     _ <- exp1a euCountries 
--     return ()
-- exp1t = exp1a threeCountries

-- exp1a :: [CountryId] -> IO String
-- -- countries is what is included in printed list 
-- exp1a countries= do
--     conn <- open dbPath 
--     let  reqYears =  [(population, Year 2024), (gnpPP, Year 2021),  (gdpPPpc, Year 2021)]

--     countryTables :: [CountryTable3] <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
--     close conn
--     let mdC = wrapMdCol3 countryTables
--     let [pop3, g3, gpc3] = mdC :: [MdColumn CountryId Double]

--     let gpc4 = (combineMdTables Divide g3  pop3){colScale=Kilo}
--         diff4 = (combineMdTables (Subtract) gpc3 gpc4)
--         mdC4 = mdC ++ [gpc4, diff4]  -- the countries with difference between GDP and GNP (cyprus, ireland, luxemburg, malta)
-- -- the operations on the tables must be with the mdcol data! 
--     let md = markdownTable allCodeNames countries mdC4  --less1m mdC
--     putStrLn md 
--     -- print md
--     return (md)

-- exp4:: IO ()  
-- -- | show only one indicator 
-- exp4 = do
--     let
--         regionDef = RBT.regionMembers -- g7, eu, russia 

--     conn <- open dbPath 
--     regionCountryTable :: RegionTable3  <- lookupRegionTable3 conn regionDef ( population) (Year 2024)
--     close conn

--     -- let aggs = map (\op -> aggregateTerry3 op regionCountryTable) [min1, median1, max1, mean1, stdDev1] 
--     -- putIOwords ["the sums are", showT aggs]
    
--     -- convert to regionTable 
--     -- make a single val for each country 
--     let rct = snd regionCountryTable :: [(RegionId, CountryTable)]
--         rct2  :: [TerryValue RegionId Double ]
--         rct2 =  sumCountryTables rct 

--     -- let  mdC = map (\(t,d) -> wrapMdCol2 d t) $ zip rcTab2rTab req
--     let  mdC = wrapMdCol ( population) rct2  
-- -- the operations on the tables must be with the mdcol data! 

--     let md = markdownTable RBT.regionNames regionOrder [mdC]

--     putStrLn md 
--     --extract one region and show the country data 
--     let regid = RegionId "EU"
--     let euMdC = getOneRegionMany3  [regionCountryTable] regid::   [MdColumn CountryId Double]
  
--     let md =  (markdownTable allCodeNames euCountries)    euMdC--less1m mdC
--     putStrLn  md 

--     return ()

-- the operations on the tables must be with the mdcol data! 
            -- let sortedRegions = sortTerryByColumn Descending  ctTab  
            -- let md = markdownTable allCodeNames sortedRegions [mdC]  --less1m mdC
            -- putStrLn md 

-- euCountries = map CountryId ["AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA"
--         ,"DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD"
--         ,"POL","PRT","ROU","SVK","SVN","ESP","SWE"]





less1m = map CountryId R3.less1mTax
-- break was 60k$ GNP 2024 and less 1 mio P 




-- exp1a :: IO ()  -- countries only 
-- | show all countries with popuplation surface and GNP
-- fig11 
exp1a_ countries= do
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
