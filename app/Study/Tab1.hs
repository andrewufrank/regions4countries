-----------------------------------------------------------------------------
--
-- Module      :   Tab1   erstes tableau
-- for each region:
-- the population, the surface, surface per person,
-- within the region: standard dev. for surface per person
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tab1
where

-- import qualified Eins.Region as RBT
-- import qualified Data.Text as T

-- import           GHC.Generics (Generic1 (to1))
-- import           GHC.IO.Handle.Types (Handle__)
-- import           R4C.Aggregate

-- import           R4C.Export.Markdown
-- import qualified Eins.Region3 as R3

import Database.SQLite.Simple
import Eins.Config
import Eins.Descriptor
import Eins.Descriptor2
import Eins.Region (regionOrder)
import Eins.Region2
import Eins.Region3
import R4C.Export.CountnryCodeNames
import R4C.Export.Table
import R4C.Import.Database
import R4C.Import.Query
import R4C.Model
import R4C.Pak
import R4C.Territory
import R4C.TerryTable
import System.FilePath ((</>))
import UniformBase hiding (uncurry, (</>))

regionMembers = regionMembers3 ++ extraRegions3 :: RegionMembers

fromPercent :: Double -> Double -> Double
fromPercent a b = a * 0.01 * b

toPercent :: Double -> Double -> Double
toPercent a b = a * 100 / b

getData11 :: IO ()
-- fig11 -- Flaeche , Nutzbare flaeche
getData11 = do
    conn <- open dbPath
    -- let reqYears =
    --         [ (population, Year 2024)
    --         , (surfaceArea, Year 2023)
    --         , (forest, Year 2023)
    --         , (urban, Year 2015)
    --         , (agriPercent, Year 2023) -- per capita, intensive
    --         ]
    -- c3 <- mapM (uncurry $ lookupCountryTable3 conn) reqYears
    -- let [pop3, surf3, forest3, urban3, agriPerc3] = c3

    pop3 <- lookupCountryTable3 conn (population) (Year 2024)
    surf3 <- lookupCountryTable3 conn (surfaceArea) (Year 2023)
    forest3 <- lookupCountryTable3 conn forest (Year 2023)
    urban3 <- lookupCountryTable3 conn urban (Year 2015)
    agriPerc3 <- lookupCountryTable3 conn agriPercent (Year 2023)

    close conn

    let -- c3 :: [(Dataset, [TerryValue CountryId (Double)])]
        c3 = [pop3, surf3, forest3, urban3, agriPerc3] :: [CountryPak3]
    -- -- <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears

    let agriFactor = scalePak 0.01 agriPerc3
        agri3 = makePakExtensive agriFactor (Year 2023) -- surfarea
        use3 = sumPak3 [forest3, urban3, agri3]

        c3' = c3 ++ [agri3, use3] :: [CountryPak3]


    -- from here to produce country table     
    let mc4 = map wrapMdCol1 c3 :: [Col CountryId (WObs Double)]

        [pop4, surf4, forest4, urban4, agriPerc4] = mc4
        agri4 = wrapMdCol1 agri3
        use4 = wrapMdCol1 use3
        
        useFactor4 =
            combineMdTables Divide use4
                surf4 {cMd = (cMd surf4) {colScale = Micro, colDecimals = 5}}

                -- combine the mdtables, to edit the cols, but no need for a dataset def 
    -- print all tables for testing
    let mdBase = markdownTable allCodeNames xcountries (mc4) -- less1m mdC
    let mdUse = markdownTable allCodeNames xcountries [surf4, agri4, forest4 , urban4, use4, useFactor4 ]
    -- make selection of region or country names automatic
    putStrLn mdBase
    putStrLn mdUse 

    -- let mc4' = mc4 ++ [agri4, use4, useFactor4]  :: [MdColumn CountryId (WObs Double)]

-- for region tables 
-- type RegionPak3 = (Dataset, TerryTable RegionId (WObs Double))  
        -- change: wobs to allow weighted average
        -- simplification: always WObs Double
        -- probably not effective, when to set the weights
        -- woud have to come from the computed aggregates

    -- make region tables
    let reg5 = country2regionPak regionMembers2 c3' :: [RegionPak3]

    --     -- usePerc4 = combinesCountryTable3 useableLandPerCent (toPercent) use4 surf4
    --     -- usePC4 = (useableLandPC, combineTerryTables (haPC) (snd use4) (snd pop4))
    let mreg5 = wrapMdCol3 reg5 --(reg4tot ++ [usePerc4, usePC4]) ::
        [pop5, surf5, forest5, urban5, agriPerc5, agri5, use5] = mreg5 
    --             [MdColumn RegionId Double]
    let mdRegion1 = markdownTable regionNames2 regionOrder2 
            [pop5, surf5, forest5, urban5, agriPerc5, agri5, use5]

            -- merge the wrap and markdownTable; they are polymorph 

    putStrLn mdRegion1


-- writeTab1Table "tab11" mdRegion

haPC a b = a * 10000 / b

-- fertilityPperyear conn = do
-- -- | compute an exensive indicator for fertility
-- --   multiply with number of woman (replace with 1/2 pop )
-- --   and assume that woman fertility is distributed over 20 years
--     let req = [population, fertilityRate]
--         years = map Year [2024,  2024, 2024]
--         reqYears = zip req years
--     c3@[pop3, fertRate3] :: [CountryPak3]
--     let women = scaleRegionTable (0.5*20) pops
--         fertilityCount3  = (fertilityCount combineTerryTables Multiply (snd women) (snd fertRate3))
--     return (fertilityCount3)

-- getData12 :: IO ()
-- -- fig12 -- bevoelkerung, wachstum migration
-- getData12 = do
--     conn <- open dbPath

--     pop3  <- lookupCountryTable3 conn (population)(Year 2024)
--     popGrowth3 <- lookupCountryTable3 conn ( population) (Year 2023)
--     fertRate3 <- lookupCountryTable3 conn fertilityRate (Year 2024)
--     mignet3 <- lookupCountryTable3 conn migrationNet (Year 2024)
--     close conn

--     let c3 :: [CountryPak3]
--         c3 = [pop3, popGrowth3, fertRate3, mignet3]

--     -- test data availability
--     let mcountry3 = wrapMdCol3 c3
--         mdCountry = markdownTable allCodeNames xcountries mcountry3  --less1m mdC
--     putStrLn mdCountry

--     --olf     conn <- open dbPath
--     -- let req = [population, populationGrowthRate, fertilityRate, migrationNet]
--     --     years = map Year [2024, 2024, 2024, 2024]
--     --     reqYears = zip req years

--     -- c3@ :: [CountryPak3]
--     --         <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
--     -- close conn

--     let -- women3 = second (scaleRegionTable (0.5*20)) pop3
--         -- fertilityCount3  = (fertilityCount, combineTerryTables (*) (snd pop3) (snd fertRate3))
--         popGrowthCount3  = (popGrowthCount, combineTerryTables (\a b -> a * b ) (snd pop3) (snd fertRate3))
--         netmigPMP3 = (netMigrationCount, combineTerryTables (\a b -> (10**6) * a / b ) (snd mignet3) (snd pop3) ) -- :: RegionTable3
--         c4 :: [CountryPak3]
--         c4 = c3 ++ [ popGrowthCount3, netmigPMP3 ]
--     -- -- let fertility2 = zip regionsList fertility

--     -- putStrLn $ show popGrowthCount3
--     let mcountry4 = wrapMdCol3 c4
--         mdCountry = markdownTable allCodeNames xcountries mcountry4  --less1m mdC
--     putStrLn mdCountry

--     let reg4  :: [(Dataset, [(RegionId, TerryTable CountryId (WObs Double))])]
--         reg4 = reg3CountryTable4 regionMembers2 c4
--         [pop4, popGrowth4, fertRate4, mignet4, popGrowthCount4, netmigPM4]
--             = reg4tot
--         reg4tot  =  regtab3_regtab1 reg4
--         reg4tot :: [(Dataset, [TerryValue RegionId Double])]

--         fertRate4x = combinesCountryTable3 fertilityRate (/) popGrowthCount4 pop4
--         mdRegion4 = wrapMdCol3 (reg4tot ++ [fertRate4x] ) :: [MdColumn RegionId Double]

--         mdRegion = markdownTable  regionNames2 regionOrder2 mdRegion4

--     putStrLn mdRegion

-- old
-- writeTab1Table "tab12" mdRegion

-- let surfPerCap = combineRegionTables Divide surfs3 pops3
-- (pops3, surfs3) <- popsSurf conn

-- netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))
-- fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))

-- -- let surfs2 = zip regionsList surfs
-- -- let netmig2 = zip regionsList netmigration
-- let netmigPC = combineRegionTables Divide netmigration pops3 :: RegionTable
-- -- let fertility2 = zip regionsList fertility
-- close conn

-- let mdCols =
--         [ surfPerCap
--         , netmigPC
--         , fertility
--         , netmigration
--         ]
-- let md = markdownTable regionNames2 regionOrder mdCols
-- putStrLn md
-- writeTab1Table "tab12" md

-- getData13 :: IO ()
-- -- | Correlation of fertility rate and net migration per capita.
-- getData13 = do
--     conn <- open dbPath
--     (pops3, _) <- popsSurf conn
--     netmigration <- aggregate regionMembers conn migrationNet (Year 2024)
--     fertility <- aggregate regionMembers conn fertilityRate (Year 2024)
--     close conn

--     let netmigPC = combineRegionTables Divide netmigration pops3
--     let fertNetmig = regionCorrelation2 ( fertility) ( netmigPC)
--     let md = unlines
--             [ " Correlation: fertility rate / net migration per capita "
--                 ++ show fertNetmig
--             ]

--     putStrLn md
--     writeTab1Table "tab13" md

-- storeTab1Tables :: IO ()
-- -- | Regenerate all Tab1 output files and update the book markdown files.
-- storeTab1Tables = do
--     getData11
--     getData12
--     getData13
--     let filename = buch </> "p30Tableaux" </> "010.Natur.md"
--     let tables = ["tab11", "tab12", "tab13"]
--     mapM_ (\tab -> writeMarkdownBlock filename filename tab (tableOutputDirectory </> tab)) tables

--     writeMarkdownBlock (buch </> "001.Natur.md") (buch </> "001.Natur.md") "tab12" (tableOutputDirectory </> "tab12")
--     writeMarkdownBlock (buch </> "001.Natur.md") (buch </> "001.Natur.md") "tab13" (tableOutputDirectory </> "tab13")

--  /home/frank/Desktop/buecher/worldFundamentals/p30Tabeleaux/010.Natur.md

-- move later somewhere
testlatest :: IO ()
testlatest = do
    conn <- open "test.sqlite"
    mObs <-
        latestObservation conn (CountryId "AUT") (dsIndicator surfaceArea)
    case mObs of
        Nothing ->
            print "nothing found"
        -- assertFailure "No surface area found"

        Just obs ->
            print $ obsYear obs

-- @?= Year 2023

regionsWithExtra :: [RegionId]
regionsWithExtra = map fst regionMembers

dreiCountr = map CountryId ["FIN", "CYP", "PRT"]
xcountries = map CountryId ["FIN", "AUT", "BRA", "USA", "RUS"]
