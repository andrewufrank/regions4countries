-----------------------------------------------------------------------------
--
-- Module      :   Tab1   erstes tableau 
-- for each region: 
-- the population, the surface, surface per person, 
-- within the region: standard dev. for surface per person

-----------------------------------------------------------------------------

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
import Study.Dataset 
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import R4C.Region3

tableOutputDirectory :: FilePath
tableOutputDirectory =
    "/home/frank/Desktop/buecher/worldFundamentals/figures"

writeTab1Table :: FilePath -> String -> IO ()
writeTab1Table filename contents = do
    createDirectoryIfMissing True tableOutputDirectory
    writeFile (tableOutputDirectory </> filename) contents

popsSurf :: p -> IO (RegionTable, RegionTable) -- ([(RegionId, Maybe Double)], [(RegionId, Maybe Double)])
popsSurf conn =  do 
    conn <- open dbPath 
    pops <-  (aggregate regionMembers conn population (Year 2024)) 
    surfs <-  (aggregate regionMembers conn surfaceArea (Year 2023)) 

    -- let 
    --         p =  zip regionsList pops  
    --         s =  zip regionsList surfs 
    return (pops,surfs)


regionMembers = regionMembers3 ++ extraRegions3 :: RegionMembers

getData11 :: IO ()
-- fig11 
getData11 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

    netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))  
    fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))  
    agrar <-  (aggregate regionMembers conn agrarland (Year 2024))  

    let surfPerCap = combineRegionTables (/) surfs3 pops3
    let surAgrarfPerCap = combineRegionTables (/) agrar pops3
    -- let netmigPC = combineRegionTables (/) netmigration pops3 
    
    close conn

    let mdCols = 
            [ MdColumn "Bevoelkerung 2024 (Mega)" Mega 6  pops3
            , MdColumn "Flaeche 2023 (Mega km²)" Mega 2  surfs3
            , MdColumn "Flaeche per capita (ha/person)" 
                    Centi 1 surfPerCap
            , MdColumn "Nutzbares Land per capita (ha/person)" 
                    Centi 0 surAgrarfPerCap 
            ]
    let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 sortedRegions mdCols
    putStrLn md 
    writeTab1Table "tab11" md

    -- compute correlation 

getData12 :: IO ()
-- fig11 
getData12 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

    netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))  
    fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))  

    -- let surfs2 = zip regionsList surfs
    let surfPerCap = combineRegionTables (/) surfs3 pops3
    -- let netmig2 = zip regionsList netmigration 
    let netmigPC = combineRegionTables (/) netmigration pops3 :: RegionTable 
    -- let fertility2 = zip regionsList fertility 
    close conn

    let mdCols = 
            [
            MdColumn "Fertilitaetsrate" Unit 2
                 fertility,
            MdColumn "Netto Migration (per Mega)" Micro 0
                netmigPC
            ]
    let sortedRegions = sortTerryByColumn Descending fertility
     -- surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 sortedRegions mdCols
    putStrLn md 
    writeTab1Table "tab12" md

    -- compute correlation fertiity and netmigPC 
    let fertNetmig = regionCorrelation2 fertility netmigPC

    putStrLn $ "correlation between fertility and net migration per capita" ++ show fertNetmig 

    return ()

getData13 :: IO ()
-- | Correlation of fertility rate and net migration per capita.
getData13 = do
    conn <- open dbPath
    (pops3, _) <- popsSurf conn
    netmigration <- aggregate regionMembers conn migrationNet (Year 2024)
    fertility <- aggregate regionMembers conn fertilityRate (Year 2024)
    close conn

    let netmigPC = combineRegionTables (/) netmigration pops3
    let fertNetmig = regionCorrelation2 fertility netmigPC
    let md = unlines
            [ " Correlation: fertility rate / net migration per capita "
                ++ show fertNetmig 
            ]

    putStrLn md
    writeTab1Table "tab13" md

storeTab1Tables :: IO ()
-- | Regenerate all Tab1 output files and update the book markdown files.
storeTab1Tables = do
    getData11
    getData12
    getData13
    let filename = buch </> "p30Tableaux" </> "010.Natur.md"
    let tables = ["tab11", "tab12", "tab13"]
    mapM_ (\tab -> writeMarkdownBlock filename filename tab (tableOutputDirectory </> tab)) tables

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
