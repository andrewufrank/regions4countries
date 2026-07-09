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

testPop = do 
    conn <- open "test.sqlite"
    pops <- mapM (showAggregate regionMembers conn population (Year 2024)) regionsList
    mapM_ print $ zip regionsList pops 
    close conn



