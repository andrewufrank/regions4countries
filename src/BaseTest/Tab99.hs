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
-- import BaseTest.Dataset 
import BaseTest.Dataset
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import BaseTest.Region
-- import R4C.Region3
-- import Study.Region2 




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

    let md = markdownTable regionNames regionOrder [pops3, surfs3]
    putStrLn md 
    writeTab1Table "tab11" md

getData12 :: IO ()
-- fig12 
getData12 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn
    close conn
    let surfPerCap = combineRegionTables Divide surfs3 pops3
        surfPerCapM = scaleRegionTable 1000 surfPerCap

    let surfpc2 = surfPerCap {colScale=Unit, colDecimals=6}
    let md = markdownTable regionNames regionOrder [pops3, surfs3, surfpc2]
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
