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
import BaseTest.Region 
import R4C.Aggregate 
import R4C.Import.Query
import R4C.Export.Table 
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import BaseTest.Config 
import BaseTest.Dataset 
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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




getData11 :: IO ()
-- fig11 
getData11 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

  
    
    close conn

    let mdCols = 
            [ MdColumn "Bevoelkerung 2024 (Mega)" Mega 6  pops3
            , MdColumn "Flaeche 2023 (Mega km²)" Mega 2  surfs3
             
            ]
    let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames sortedRegions mdCols
    putStrLn md 
    writeTab1Table "tab11" md

getData12 :: IO ()
-- fig12 
getData12 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

  
    
    close conn

    let mdCols = 
            [ MdColumn "fig 12  2024 (Mega)" Mega 0  pops3
            , MdColumn "fig 12  2023 (Mega km²)" Mega 0 surfs3
             
            ]
    let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames sortedRegions mdCols
    putStrLn md 
    writeTab1Table "tab12" md


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
