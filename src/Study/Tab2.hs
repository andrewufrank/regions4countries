-----------------------------------------------------------------------------
--
-- Module      :   Tab2 ernaehrung 
-- for each region: 
-- the population, the surface, surface per person, 
-- within the region: standard dev. for surface per person

-----------------------------------------------------------------------------

module Study.Tab2
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
import R4C.Region3
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Study.Tab1 



writeTab2Table :: FilePath -> String -> IO ()
writeTab2Table filename contents = do
    createDirectoryIfMissing True tableOutputDirectory
    writeFile (tableOutputDirectory </> filename) contents

-- popsSurf :: p -> IO (RegionTable, RegionTable) -- ([(RegionId, Maybe Double)], [(RegionId, Maybe Double)])
-- popsSurf conn =  do 
--     conn <- open dbPath 
--     pops :: RegionTable <-  (aggregate regionMembers2 conn population (Year 2024))  
--     surfs <-  (aggregate regionMembers2 conn surfaceArea (Year 2023)) 

--     return (pops,surfs)

getData21 :: IO ()
-- fig21 cereal production and veg. consumption 
getData21 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

    cerealProd <- aggregate regionMembers2 conn cerealProduction (Year 2023) -- 2024 not all values 
    let cerealFood = scaleRegionTable 0.1 pops3   -- 100 kg per head   
    
    close conn

    let mdCols = 
            [ cerealProd
            , cerealFood
            --  MdColumn "Getreideproduktion (T kg)" Mega 2  cerealProd
            -- -- value is t
            -- , MdColumn "menschliche Ernaehrung (Mega kg)" Mega 2  cerealFood
            -- value is 10**11 kg 
            ]
    -- let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 regionOrder mdCols
    putStrLn md
    writeTab2Table "tab21" md

getData22 :: IO ()
-- | Ernaehrungssituation 1980 (ohne Russland, noch nicht existent)
getData22 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn
    pops1980 <-  (aggregate regionMembers2 conn population (Year 1980)) 
    cerealProd <- aggregate regionMembers2 conn cerealProduction (Year 1980) -- 2024 not all values 
    let cerealFood = scaleRegionTable 0.1 pops1980   -- 100 kg per head   
    let cerealDomUse = scaleRegionTable (2.5) cerealFood -- 40..45% for human food 
    let potExport = combineRegionTables Subtract cerealProd cerealDomUse 

    close conn

    let mdCols = 
            [ cerealProd 
            , cerealFood 
            , cerealDomUse 
            , potExport 
            --  MdColumn "Getreideproduktion (T kg)" Mega 0  cerealProd
            -- -- value is t
            -- , MdColumn "menschliche Ernaehrung (Mega kg)" Mega 0  cerealFood
            -- -- value is 10**11 kg 
            -- , MdColumn "total Verbrauch (Mega kg)" Mega 0 cerealDomUse 
            -- , MdColumn "potential fuer Export (Mega kg)" Mega 0 potExport

            ]
    -- let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 regionOrder mdCols
    putStrLn md
    writeTab2Table "tab22" md

-- duengerverbrauch und produktion 
getData23 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

    arablHA  <- aggregate regionMembers2 conn agriculturalLand (Year 2023)
    fertConsumpha <- aggregate regionMembers2 conn ferilizerConsum (Year 2023)
    let fertilizerConsumTot = combineRegionTables Multiply arablHA fertConsumpha 
    
    -- fertConsumpc <- aggregate regionMembers2 conn ferilizerConsum2 (Year 2023) -- leer
    -- let fertilizerProd = combineRegionTables Divide fertilizerConsumTot fertConsumpc 
    -- TODO must be handled with virtual dataset

    close conn

    let mdCols = 
            [ arablHA
            , fertConsumpha
            , fertilizerConsumTot 
            -- , fertConsumpc 
            -- , fertilizerProd 
            -- MdColumn "arablHA (M ha)" Mega 0  arablHA
            -- , MdColumn "fertConsum (kg/ha)" Unit 0  fertConsumpha
            -- , MdColumn "fertilizerConsum Tot(G kg)" Giga 0  fertilizerConsumTot
            -- , MdColumn "Duengerverbrauch (% der Produktion)" Unit 0  fertConsumpc
            -- , MdColumn "Duengerproduktion (M kg)" Mega 0  fertilizerProd
            -- value is t
            -- value is 10**11 kg 
            ]
    -- let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 regionOrder mdCols
    putStrLn md
    writeTab2Table "tab23" md

storeTab2Tables :: IO ()
storeTab2Tables = do
    -- getData21
    getData22
    getData23
    let filename = buch </> "p30Tableaux" </> "020ernaehung.md"
    let tables = ["tab22", "tab23"]
    mapM_ (\tab -> writeMarkdownBlock filename filename tab (tableOutputDirectory </> tab)) tables
