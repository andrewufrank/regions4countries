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
import R4C.Query
import R4C.Table 
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 
import Study.Dataset 
import R4C.Statistics

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

    netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))  
    fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))  

    -- let surfs2 = zip regionsList surfs
    let surfPerCap = combineRegionTables (/) surfs3 pops3
    -- let netmig2 = zip regionsList netmigration 
    let netmigPC = combineRegionTables (/) netmigration pops3 
    -- let fertility2 = zip regionsList fertility 
    close conn

    let mdCols = 
            [ MdColumn "Population 2024 (M)" 1000000 6  pops3
            , MdColumn "Surface 2023 (M km²)" 1000000 2  surfs3
            , MdColumn "Surface per capita (ha/person)" 0.01 1 surfPerCap
            ]
    let sortedRegions = sortRegionsByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable sortedRegions mdCols
    putStrLn md 

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
            MdColumn "Fertilitaetsrate" 1 2
                 fertility,
            MdColumn "Netto Migration (per M)" 0.000001 0
                netmigPC
            ]
    let sortedRegions = sortRegionsByColumn Descending fertility
     -- surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable sortedRegions mdCols
    putStrLn md 

    -- compute correlation fertiity and netmigPC 
    let fertNetmig = regionCorrelation fertility netmigPC

    putStrLn $ "correlation between fertility and net migration per capita" ++ show fertNetmig 

    return ()

 

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
