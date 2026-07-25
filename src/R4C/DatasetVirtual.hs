-----------------------------------------------------------------------------
--
-- Module      :  Virtual Datasets
--
-- datasets which are derived from others with the descriptor set 
-- used primarily for the datasets which are not extensional 
-- and cannot easily summed
-----------------------------------------------------------------------------

module R4C.DatasetVirtual 
    where 

import R4C.Model 
import Database.SQLite.Simple
import R4C.Aggregate
import Study.Dataset 
import BaseTest.Region
import R4C.Export.Table 

surfacePerCapita :: Connection -> IO (MdColumn RegionId Double)
-- | a virtual dataset for surface per capita (per region)
surfacePerCapita conn = do 
    pops <-  (aggregate regionMembers conn population (Year 2024)) 
    surfs <-  (aggregate regionMembers conn surfaceArea (Year 2023)) 
    let surfPerCap = combineRegionTables Divide surfs pops
    let surfpc2 = surfPerCap {colScale=Unit, colDecimals=6}  -- Mega/Mega
    return surfpc2 

usableAreaPerCapita :: Connection -> IO (MdColumn RegionId Double)
-- | a virtual dataset for the useful area ha per capita (per region)
-- todo add other areas or use agriculturalLand per cent of total surface
usableAreaPerCapita conn = do 
    pops <-  (aggregate regionMembers conn population (Year 2024)) 
    arabl <-  (aggregate regionMembers conn arableLand (Year 2023))  -- add pastures, forest, urban 

    let surfPerCap = combineRegionTables Divide arabl pops
    let surfpc2 = surfPerCap {colScale=Unit, colDecimals=6}  -- Mega/Mega
    return surfpc2 
