-----------------------------------------------------------------------------
--
-- Module      :   Tab1   erstes tableau
-- for each region:
-- the population, the surface, surface per person,
-- within the region: standard dev. for surface per person
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tab1 where

import Database.SQLite.Simple
import Eins.Config
import Eins.Descriptor
import Eins.Region (regionOrder)
import Eins.Region2
import Eins.Region3
import R4C.Country (lookupCountries)
import R4C.Export.CountnryCodeNames
import R4C.Export.Table
import R4C.Import.Query
import R4C.Model
import R4C.Pak
import R4C.TerryTable
import System.FilePath ((</>))
import UniformBase hiding (uncurry, (</>))

regionMembers :: RegionMembers
regionMembers = regionMembers3 ++ extraRegions3 :: RegionMembers

fromPercent :: Double -> Double -> Double
fromPercent a b = a * 0.01 * b

toPercent :: Double -> Double -> Double
toPercent a b = a * 100 / b

getData11 :: IO ()
-- fig11 -- Flaeche , Nutzbare flaeche
getData11 = do
    conn <- open dbPath
    pop3 <- lookupCountryPak conn (population) (Year 2024)
    surf3 <- lookupCountryPak conn (surfaceArea) (Year 2023)
    forest3 <- lookupCountryPak conn forest (Year 2023)
    urban3 <- lookupCountryPak conn urban (Year 2015)

        -- for tab2
    agriPerc3 <- lookupCountryPak conn agriPercent (Year 2023)
    fertRate3 <- lookupCountryPak conn fertilityRate (Year 2024)
    mignet3 <- lookupCountryPak conn migrationNet (Year 2024)
    popGrowthPercent3 <- lookupCountryPak conn populationGrowthRate (Year 2024)
    close conn

    let c3 = [pop3, surf3, forest3, urban3, agriPerc3
                , fertRate3, mignet3, popGrowthPercent3] :: [CountryPak3]

-- test data availability
        mdCountry = markdownPakTable allCodeNames xcountries c3 -- less1m mdC
    putStrLn mdCountry

    let  -- construct new paks
        agri3 = setPakShortName "Landwirtschaft" $
                combinePaks FromPercentOf agriPerc3 surf3
        -- could take the base dataset from the weighted value
        use3 = setPakShortName "Nutzbar" $ sumPaks [forest3, urban3, agri3]
        useF3 = setPakShortName "Nutzbar" $
                combinePaks ToPercentOf use3 surf3
        usePC3 = setPakUnitScale "ha/P" . scalePak 1000 $   combinePaks Divide use3 pop3 

    let     -- for tab2
        netmigPMP3 = setPakUnitScale "Milli" . setPakShortName "Migrationsrate " 
            . scalePak (10**3) $ combinePaks Divide mignet3 pop3
        popGrowth3 = setPakShortName "Wachstum" $ makePakExtensive popGrowthPercent3 (Year 2024)

    let c31 = [agri3, use3, useF3, usePC3] 
        c32 = [netmigPMP3, popGrowth3]

        c3' = concat [c3, c31, c32] :: [CountryPak3]

    -- from here to produce country table

    let mdBase = markdownPakTable allCodeNames xcountries c3
    let mdUse1 = markdownPakTable allCodeNames xcountries c31
        mdUse2 = markdownPakTable allCodeNames xcountries c32
        
    let tab11paks = [surf3, agri3, forest3, urban3, use3, useF3, usePC3]
        tab11 = markdownPakTable allCodeNames xcountries tab11paks

    let tab12paks = [pop3, fertRate3, popGrowthPercent3, popGrowth3,  mignet3, netmigPMP3]
        tab12 =  markdownPakTable allCodeNames xcountries tab12paks

    -- make selection of region or country names automatic
    putIOwords ["base\n", s2t mdBase]
    putIOwords ["use1\n", s2t mdUse1]
    putIOwords ["use2\n", s2t mdUse2]
    putIOwords ["tab11\n", s2t tab11]
    putIOwords ["tab12\n", s2t tab12]


    -- make region tables

    -- let reg5 = countryToRegionPaks regionMembers2 tab11paks :: [RegionPak3]
    -- let mdRegion1 = markdownPakTable regionNames2 regionOrder2 reg5
    -- putIOwords ["the tables\n", s2t mdRegion1]

    let tabNames = ["c3", "c31", "c32", "tab11", "tab12"] :: [Text]
        tables = zip tabNames [c3, c31, c32, tab11paks, tab12paks] :: [(Text, [CountryPak3])]
        regionTables = map makeRegionTables tables
    putIOwords ["the tables\n", unlines'  regionTables]
    

-- writeTab1Table "tab11" mdRegion
makeCountryTables :: (Text ,  [CountryPak3]) -> Text
makeCountryTables (name, regs) = "\n" <> name <> "\n" 
            <> s2t  (markdownPakTable allCodeNames xcountries regs)


-- makeRegionTabs :: [Pak CountryId (WObs Double)] -> String
makeRegionTables :: (Text ,  [CountryPak3]) -> Text
makeRegionTables (name, regs) = "\n" <> name <> "\n" <> s2t  (markdownPakTable regionNames2 regionOrder2 
            $ countryToRegionPaks regionMembers2 regs)


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

--     pop3 <- lookupCountryPak conn (population) (Year 2024)
--     fertRate3 <- lookupCountryPak conn fertilityRate (Year 2024)
--     mignet3 <- lookupCountryPak conn migrationNet (Year 2024)
--     popGrowthPercent3 <- lookupCountryPak conn populationGrowthRate (Year 2024)
--     close conn

--     let c3 :: [CountryPak3]
--         c3 = [pop3, fertRate3, mignet3, popGrowthPercent3]

--     -- test data availability
--     -- let mcountry3 = map wrapMdCol1 c3
--         mdCountry = markdownPakTable allCodeNames xcountries c3 -- less1m mdC
--     putStrLn mdCountry

--     let
--         -- women3 = second (scaleRegionTable (0.5*20)) pop3
--         -- fertilityCount3  = (fertilityCount, combineTerryTables (*) (snd pop3) (snd fertRate3))
--         -- popGrowthCount3 = combinePaks Multiply pop3 fertRate3
--         netmigPMP3 = setPakUnitScale "Milli" . setPakShortName "Migrationsrate " 
--             . scalePak (10**3) $ combinePaks Divide mignet3 pop3
--         popGrowth3 = setPakShortName "Wachstum" $ makePakExtensive popGrowthPercent3 (Year 2024)

--         c4 :: [CountryPak3]
--         c4 = [pop3, fertRate3, popGrowthPercent3, popGrowth3,  mignet3, netmigPMP3]

--     -- putStrLn $ show popGrowthCount3
--     let mcountry4 = map wrapMdCol1 c4
--         mdCountry = markdownTable allCodeNames xcountries mcountry4 -- less1m mdC
--     putStrLn mdCountry

--     -- let [pop4,  fertRate4, mignet4, popGrowthCount4, popGrowth4, netmigPM4] =
--             -- reg4tot
--     let reg4tot = countryToRegionPaks regionMembers2 c4 :: [RegionPak3]

--         -- fertRate4x = combinePaks Divide popGrowthCount4 pop4
--         -- mdRegion4 =
--         --     map wrapMdCol1 (reg4tot ++ [fertRate4x]) ::
--         --         [Col RegionId (WObs Double)]

--     let mdRegion = markdownPakTable regionNames2 regionOrder2 reg4tot

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
