-----------------------------------------------------------------------------
--
-- Module      :   Tables for test 
-- for each region: 
-- the population, the surface, surface per person, 
-- within the region: standard dev. for surface per person

-----------------------------------------------------------------------------

module BaseTest.Tab99
    where

import R4C.Model 
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator 
-- import R4C.Region3 
import R4C.Aggregate 
import R4C.Import.Query
import R4C.Export.Table 
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import BaseTest.Config 
-- import BaseTest.Dataset1
import Study.Dataset
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import BaseTest.Region
-- import R4C.Region3
-- import Study.Region2 
import R4C.DatasetVirtual



writeTab1Table :: FilePath -> String -> IO ()
writeTab1Table filename contents = do
    createDirectoryIfMissing True tableOutputDirectory
    writeFile (tableOutputDirectory </> filename) contents

popsSurf :: p -> IO (RegionTable, RegionTable) -- ([(RegionId, Maybe Double)], [(RegionId, Maybe Double)])
popsSurf conn =  do 
    conn <- open dbPath 
    pops <-  (aggregate regionMembers conn population (Year 2024)) 
    surfs <-  (aggregate regionMembers conn surfaceArea (Year 2023)) 

    return (pops,surfs)

getData11 :: IO ()
getData11 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn
    close conn

    let surfPC = combineRegionTables Divide surfs3 pops3 
    let md = markdownTable regionNames regionOrder [pops3, surfs3, surfPC]
    putStrLn md 
    writeTab1Table "tab11" md

usableAreaPerCapita1 :: Connection -> IO (MdColumn RegionId Double)
-- | a virtual dataset for the useful area ha per capita (per region)
usableAreaPerCapita1 conn = do 
    pops <-  (aggregate regionMembers conn population (Year 2024)) 
    arabl <-  (aggregate regionMembers conn arableLand (Year 2023))  -- add pastures, forest, urban 
            -- ersetzt durch agrarlandPart

    let surfPerCap = combineRegionTables Divide arabl pops
    let surfpc2 = surfPerCap {colScale=Unit, colDecimals=6}  -- Mega/Mega
    return surfpc2 

getData12 :: IO ()
-- fig12 
getData12 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn
    surfpc2 <- surfacePerCapita conn
    arablpc <- usableAreaPerCapita1 conn

    close conn
    -- let surfPerCap = combineRegionTables Divide surfs3 pops3
    --     -- surfPerCapM = scaleRegionTable (10**6) surfPerCap -- convert km2 to m2

    -- let surfpc2 = surfPerCap {colScale=Unit, colDecimals=6}  -- Mega/Mega
    let md = markdownTable regionNames regionOrder [pops3, surfs3, surfpc2, arablpc]
    putStrLn md 
    writeTab1Table "tab12" md

fertilityPperyear conn = do 
-- | compute an exensional indicator for fertility  
--   multiply with number of woman (replace with 1/2 pop )
    pops <-  (aggregate regionMembers conn population (Year 2024)) 
    fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))  
    let women = scaleRegionTable (0.5) pops
        fertilityCount  = combineRegionTables Multiply women fertility 
    return (fertilityCount)

-- agrarlandPC :: Connection -> IO (MdColumn RegionId Double)
-- issue with weighted
-- agrarlandPC conn = do 
-- -- agrarland (arable, permant crops or pasture) per capita
--     pops <- aggregate regionMembers conn population (Year 2024)
--     surf <- aggregate regionMembers conn surfaceArea (Year 2023)
--     agrarPart <-  (aggregate regionMembers conn agrarlandPart (Year 2023))  -- non-extensional
--     let agrarPerc = scaleRegionTable (0.01) agrarPart  -- convert % to factor
--         agrarTotal = combineRegionTables Multiply agrarPerc surf
--         agrarsurfPerCap = combineRegionTables Divide agrarTotal pops
--     let surfpc2 = agrarsurfPerCap {colScale=Unit, colDecimals=6}

--     return surfpc2 


getData13 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

    usablePC <- usableAreaPerCapita conn
    -- agrarPC <- agrarlandPC conn  -- TODO 

    -- netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))  
    -- fertilityPyear <-  fertilityPperyear conn 
    -- agrar <-  (aggregate regionMembers conn agrarland (Year 2023))    -- nur ackerland!
    -- agrarPC <-  (aggregate regionMembers conn agriculturalLandPC (Year 2023))  
            -- wheigted!
    close conn

    let mdCols = 
            [ pops3
            , surfs3
            , usablePC 
            -- , agrarPC
            ]
    let md = markdownTable regionNames regionOrder mdCols
    putStrLn md 



storeTables :: IO ()
-- | Regenerate all Tab1 output files and update the book markdown files.
storeTables = do
    getData11
    getData12
    let filename = buch </> "p99Tableaux" </> "099test.md"
    let tables = ["tab11", "tab12" ]
    mapM_ (\tab -> writeMarkdownBlock filename filename tab (tableOutputDirectory </> tab)) tables

 

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
