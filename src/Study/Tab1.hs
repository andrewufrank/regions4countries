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

getData = do 
    conn <- open "test.sqlite"
    pops <- mapM (aggregate regionMembers conn population (Year 2024)) regionsList
    let pops2 =  zip regionsList pops 
    mapM_ print $ pops2
    surfs <- mapM (aggregate regionMembers conn surfaceArea (Year 2023)) regionsList
    let surfs2 =  zip regionsList surfs 
    mapM_ print $ surfs2

    let surfPerCap = combineRegionTables (/) surfs2 pops2
    mapM_ print $ surfPerCap

    close conn    

-- spc <- surfacePerCapita surface population      -- km²/person

-- pd  <- populationDensity population surface     -- persons/km²

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