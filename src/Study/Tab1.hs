-----------------------------------------------------------------------------
--
-- Module      :   Tab1   erstes tableau 
-- for each region: 
-- the population, the surface, surface per person, 
-- within the region: standard dev. for surface per person

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Study.Tab1
    where

import R4C.Model
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator 
import Study.Region2 
import R4C.Aggregate 
import R4C.Import.Query
import R4C.Export.Table 
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 
import Study.Descriptor 
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import R4C.Region3
import R4C.Model
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator 
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 
import Study.Descriptor
import Study.Region2
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


writeTab1Table :: FilePath -> String -> IO ()
writeTab1Table filename contents = do
    createDirectoryIfMissing True tableOutputDirectory
    writeFile (tableOutputDirectory </> filename) contents

-- popsSurf :: p -> IO (RegionTable, RegionTable) -- ([(RegionId, Maybe Double)], [(RegionId, Maybe Double)])
-- popsSurf conn =  do 
--     conn <- open dbPath 
--     pops <-  (aggregate regionMembers conn population (Year 2024)) 
--     surfs <-  (aggregate regionMembers conn surfaceArea (Year 2023)) 

--     -- let 
--     --         p =  zip regionsList pops  
--     --         s =  zip regionsList surfs 
--     return (pops,surfs)


regionMembers = regionMembers3 ++ extraRegions3 :: RegionMembers

-- should become a virtual dataset 
agriLand :: Dataset
agriLand =
    Dataset
        { dsShortName = "Landwirtschaft"
        , dsName = "Surface area (sq. km)"
        , dsDefinition =
            " ."
        , dsUnit = "km\178"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = True
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
        --     "FAO  "
        }
useableLand :: Dataset
useableLand =
    Dataset
        {  dsShortName = "Nutzbares Land"
        , dsName = "useable Land area (sq. km)"
        -- , dsDefinition =
        --     " ."
        , dsUnit = "km\178"
        -- , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = True
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }
useableLandPerCent :: Dataset
useableLandPerCent =
    Dataset
        {  dsShortName = "Nutzbares Land"
        , dsName = "useableLand PerCent"
        -- , dsDefinition =
        --     " ."
        , dsUnit = "%"
        -- , dsAggregation = Sum
        , dsDecimals = 2
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }

useableLandPC:: Dataset
useableLandPC =
    Dataset
        {  dsShortName = "Nutzbare Landflaeche"
        , dsName = "useableLand per Capita"
        -- , dsDefinition =
        --     " ."
        , dsUnit = "a/P"
        -- , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }

fromPercent :: Double -> Double -> Double
fromPercent a b = a * 0.01 * b 

toPercent :: Double -> Double -> Double 
toPercent a b = a * 100 / b 

getData11 :: IO ()
-- fig11 -- Flaeche , Nutzbare flaeche 
getData11 = do
    conn <- open dbPath 
    let req = [population, surfaceArea, forest, urban, agriPercent]
        years = map Year [2023,  2023, 2023, 2015, 2023]
        reqYears = zip req years
    c3@[pop3, surf3, forest3, urban3, agriPerc3] :: [CountryTable3] 
            <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears

    close conn     

    -- compute agricultural land area 
    let agri3 :: CountryTable3 
        agri3 = (agriLand, combineTerryTables (fromPercent) (snd agriPerc3) (snd surf3))
        use3 = (useableLand, sumTerryTables $ map snd [forest3, urban3, agri3])
        c4 = c3 ++ [agri3, use3]:: [CountryTable3]

        mcountry4 = wrapMdCol3 c4
        -- print all tables for testing   
    let mdCountry = markdownTable allCodeNames xcountries mcountry4  --less1m mdC
    putStrLn mdCountry 

    -- make region tables
    let reg4 = reg3CountryTable4 regionMembers2 c4 :: [(Dataset, [(RegionId, TerryTable CountryId Double)])]
        reg4tot@[pop4, surf4,forest4,urban4,agriPerc4, agri4, use4]
                 =  regtab3_regtab1 reg4 :: [(Dataset, [TerryValue RegionId Double])]
        usePerc4 = combinesCountryTable3 useableLandPerCent (toPercent)  use4 surf4
        usePC4 = combinesCountryTable3 useableLandPC (haPC) use4 pop4 
        mdRegion4 = wrapMdCol3 (reg4tot ++ [usePerc4, usePC4]) :: [MdColumn RegionId Double]

        mdRegion = markdownTable  regionNames2 regionOrder2 mdRegion4

    putStrLn mdRegion
    writeTab1Table "tab11" mdRegion 

haPC a b = a * 10000 / b 

    -- -- od 
    -- (pops3, surfs3) <- popsSurf conn

    -- netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))  
    -- -- fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))  
    -- agrar <-  (aggregate regionMembers conn arableLand (Year 2023))    -- nur ackerland!
    -- -- agrarPC <-  (aggregate regionMembers conn agriculturalLandPC (Year 2023))  
    --         -- wheigted!
    -- close conn

    -- let mdCols = 
    --         [ pops3
    --         , surfs3
    --         -- , agrarPC
    --         , agrar
    --         ]
    -- let md = markdownTable regionNames2 regionOrder2 mdCols
    -- putStrLn md 
    -- writeTab1Table "tab11" md

-- getData12 :: IO ()
-- -- fig11 
-- getData12 = do
--     conn <- open dbPath 
--     (pops3, surfs3) <- popsSurf conn

--     netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))  
--     fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))  

--     -- let surfs2 = zip regionsList surfs
--     let surfPerCap = combineRegionTables Divide surfs3 pops3
--     -- let netmig2 = zip regionsList netmigration 
--     let netmigPC = combineRegionTables Divide netmigration pops3 :: RegionTable 
--     -- let fertility2 = zip regionsList fertility 
--     close conn

--     let mdCols = 
--             [ surfPerCap
--             , netmigPC
--             , fertility
--             , netmigration
--             ]
--     let md = markdownTable regionNames2 regionOrder mdCols
--     putStrLn md 
--     writeTab1Table "tab12" md

-- getData13 :: IO ()
-- -- | Correlation of fertility rate and net migration per capita.
-- getData13 = do
--     conn <- open dbPath
--     (pops3, _) <- popsSurf conn
--     netmigration <- aggregate regionMembers conn migrationNet (Year 2024)
--     fertility <- aggregate regionMembers conn fertilityRate (Year 2024)
--     close conn

--     let netmigPC = combineRegionTables Divide netmigration pops3
--     let fertNetmig = regionCorrelation2 ( fertility) ( netmigPC)
--     let md = unlines
--             [ " Correlation: fertility rate / net migration per capita "
--                 ++ show fertNetmig 
--             ]

--     putStrLn md
--     writeTab1Table "tab13" md

-- storeTab1Tables :: IO ()
-- -- | Regenerate all Tab1 output files and update the book markdown files.
-- storeTab1Tables = do
--     getData11
--     getData12
--     getData13
--     let filename = buch </> "p30Tableaux" </> "010.Natur.md"
--     let tables = ["tab11", "tab12", "tab13"]
--     mapM_ (\tab -> writeMarkdownBlock filename filename tab (tableOutputDirectory </> tab)) tables

--     writeMarkdownBlock (buch </> "001.Natur.md") (buch </> "001.Natur.md") "tab12" (tableOutputDirectory </> "tab12")
--     writeMarkdownBlock (buch </> "001.Natur.md") (buch </> "001.Natur.md") "tab13" (tableOutputDirectory </> "tab13")

--  /home/frank/Desktop/buecher/worldFundamentals/p30Tabeleaux/010.Natur.md

-- move later somewhere 
testlatest :: IO () 
testlatest = do 
    conn <- open "test.sqlite" 
    mObs <- latestObservation conn (CountryId "AUT") (dsIndicator surfaceArea)
    case mObs of
        Nothing ->
            print "nothing found" 
            --assertFailure "No surface area found"

        Just obs ->
            print  $ obsYear obs 
            -- @?= Year 2023

regionsWithExtra :: [RegionId] 
regionsWithExtra = map fst regionMembers 

dreiCountr = map CountryId ["FIN", "CYP", "PRT"]
xcountries = map CountryId ["FIN", "CYP", "PRT", "AUT", "BRA", "BGD"]

