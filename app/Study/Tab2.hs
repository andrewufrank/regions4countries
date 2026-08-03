-----------------------------------------------------------------------------
--
-- Module      :   Tab2 ernaehrung 
-- for each region: 
-- the population, the surface, surface per person, 
-- within the region: standard dev. for surface per person

-----------------------------------------------------------------------------

module Tab2
    where

import Eins.Config
import R4C.Export.Markdown (writeMarkdownBlock)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UniformBase hiding ((</>))


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
-- MIGRATION TODO: cerealProduction and the old region-table scaling API do
-- not exist in the current descriptor/Pak API.  Restore this table after a
-- Dataset descriptor for cereal production has been added.
getData21 = pure ()

getData22 :: IO ()
-- | Ernaehrungssituation 1980 (ohne Russland, noch nicht existent)
-- MIGRATION TODO: see getData21.  The original implementation also depends
-- on the removed scaleRegionTable/combineRegionTables functions.
getData22 = pure ()

-- duengerverbrauch und produktion 
-- MIGRATION TODO: arableLandPC and ferilizerConsum descriptors are not
-- present in the current descriptor modules.
getData23 :: IO ()
getData23 = pure ()

storeTab2Tables :: IO ()
storeTab2Tables = do
    -- MIGRATION TODO: re-enable these lines together with getData21-23.
    -- getData21
    -- getData22
    -- getData23
    -- let filename = buch </> "p30Tableaux" </> "020ernaehung.md"
    -- let tables = ["tab22", "tab23"]
    -- mapM_ (\tab -> writeMarkdownBlock filename filename tab (tableOutputDirectory </> tab)) tables
    pure ()
