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
import Study.Indicator 
import Study.Region2 
import R4C.Aggregate 
import R4C.Query
import R4C.Table 
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 

popsSurf :: p -> IO ([(RegionId, Maybe Double)], [(RegionId, Maybe Double)])
popsSurf conn =  do 
    conn <- open dbPath 
    pops <- mapM (aggregate regionMembers conn population (Year 2024)) regionsList
    surfs <- mapM (aggregate regionMembers conn surfaceArea (Year 2023)) regionsList

    let 
            p =  zip regionsList pops  
            s =  zip regionsList surfs 
    return (p,s)




getData11 :: IO ()
-- fig11 
getData11 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

    netmigration <- mapM (aggregate regionMembers conn migrationNet (Year 2024)) regionsList 
    fertility <- mapM (aggregate regionMembers conn fertilityRate (Year 2024)) regionsList 

    -- let surfs2 = zip regionsList surfs
    let surfPerCap = combineRegionTables (/) surfs3 pops3
    let netmig2 = zip regionsList netmigration 
    let netmigPC = combineRegionTables (/) netmig2 pops3 
    let fertility2 = zip regionsList fertility 
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

getData12 :: IO ()
-- fig11 
getData12 = do
    conn <- open dbPath 
    (pops3, surfs3) <- popsSurf conn

    netmigration <- mapM (aggregate regionMembers conn migrationNet (Year 2024)) regionsList 
    fertility <- mapM (aggregate regionMembers conn fertilityRate (Year 2024)) regionsList 

    -- let surfs2 = zip regionsList surfs
    let surfPerCap = combineRegionTables (/) surfs3 pops3
    let netmig2 = zip regionsList netmigration 
    let netmigPC = combineRegionTables (/) netmig2 pops3 
    let fertility2 = zip regionsList fertility 
    close conn

    let mdCols = 
            [
            MdColumn "Fertilitaetsrate" 1 2
                 fertility2,
            MdColumn "Netto Migration (per M)" 0.000001 0
                netmigPC
            ]
    let sortedRegions = sortRegionsByColumn Descending fertility2 -- surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable sortedRegions mdCols
    putStrLn md 




-- move later somewhere 
testlatest :: IO () 
testlatest = do 
    conn <- open "test.sqlite" 
    mObs <- latestObservation conn (CountryId "AUT") (indicatorId surfaceArea)
    case mObs of
        Nothing ->
            print "nothing found" 
            --assertFailure "No surface area found"

        Just obs ->
            print  $ obsYear obs 
            -- @?= Year 2023
