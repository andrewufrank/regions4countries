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

getData :: IO ()
getData = do
    conn <- open "test.sqlite"

    pops <- mapM (aggregate regionMembers conn population (Year 2024)) regionsList
    surfs <- mapM (aggregate regionMembers conn surfaceArea (Year 2023)) regionsList

    let pops2 = zip regionsList pops :: DTable
    let surfs2 = zip regionsList surfs
    let surfPerCap = combineRegionTables (/) surfs2 pops2

    close conn

    let t1 = markdownTable regionsList
            [ MdColumn "Population 2024 (M)" 1000000 1 ( pops2)
            , MdColumn "Surface 2023 (M km²)" 1000000 2 ( surfs2)
            , MdColumn "Surface per capita (ha/person)" 0.01 1 ( surfPerCap)
            ]
    putStrLn t1 

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