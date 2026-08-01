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

-- import           Eins.Region (regionOrder)
import qualified Eins.Region as RBT
import qualified Data.Text as T
import           Database.SQLite.Simple
import           GHC.Generics (Generic1 (to1))
import           GHC.IO.Handle.Types (Handle__)
import           R4C.Aggregate
import           R4C.Export.CountnryCodeNames
import           R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import           R4C.Export.Table
import           R4C.Import.Database
import           R4C.Import.Query
import           R4C.Model
import           R4C.Pak
import qualified Eins.Region3 as R3
import           Eins.Region3
import           R4C.Statistics
import           R4C.Territory
import           Eins.Config
import           Eins.Descriptor
import           Eins.Region2
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           UniformBase hiding ((</>))
import Eins.Descriptor2 

-- writeTab1Table :: FilePath -> String -> IO ()
-- writeTab1Table filename contents = do
--     createDirectoryIfMissing True tableOutputDirectory
--     writeFile (tableOutputDirectory </> filename) contents

-- popsSurf :: p -> IO (RegionTable, RegionTable) -- ([(RegionId, Maybe Double)], [(RegionId, Maybe Double)])
-- popsSurf conn =  do
--     conn <- open dbPath
--     pops <-  (aggregate regionMembers conn population (Year 2024))
--     surfs <-  (aggregate regionMembers conn surfaceArea (Year 2023))

--     -- let
--     --         p =  zip regionsList pops
--     --         s =  zip regionsList surfs
--     return (pops,surfs)


regionMembers = regionMembers3 ++ extraRegions3 :: RegionMembers

fromPercent :: Double -> Double -> Double
fromPercent a b = a * 0.01 * b

toPercent :: Double -> Double -> Double
toPercent a b = a * 100 / b


getData11 :: IO ()
-- fig11 -- Flaeche , Nutzbare flaeche
getData11 = do
    conn <- open dbPath

    pop3  <- lookupCountryTable3 conn (population)(Year 2024)
    surf3 <- lookupCountryTable3 conn ( surfaceArea) (Year 2023)
    forest3 <- lookupCountryTable3 conn forest (Year 2023)
    urban3 <- lookupCountryTable3 conn urban (Year 2015)
    agriPerc3 <- lookupCountryTable3 conn agriPercent (Year 2023)
    close conn

    let c3 :: [(Dataset, [TerryValue CountryId (WObs Double)])] 
        c3 = [pop3, surf3, forest3, urban3, agriPerc3] :: [CountryTable3]
            -- <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears

    -- compute agricultural land area
    let agri3 :: CountryTable3
        -- agri3 = (agriLand, combineTerryTables (fromPercent) (snd agriPerc3) (snd surf3))
        agri3 = (agriland, makeTerryTableExtensive agriPerc3)
        use3 = (useableLand, sumTerryTables $ map snd [forest3, urban3, agri3])
        c4 = c3 ++ [agri3, use3]:: [CountryTable3]

        mcountry4 = wrapMdCol3 c4
        -- print all tables for testing
    let mdCountry = markdownTable allCodeNames xcountries mcountry4  --less1m mdC
    putStrLn mdCountry

    -- make region tables
    let reg4 = reg3CountryTable4 regionMembers2 c4 
    -- :: [(Dataset, [(RegionId, TerryTable CountryId Double)])]
        reg4tot@[pop4, surf4,forest4,urban4,agriPerc4, agri4, use4]
                 =  regtab3_regtab1 reg4 -- :: [(Dataset, [TerryValue RegionId Double])]
        usePerc4 = combinesCountryTable3 useableLandPerCent (toPercent)  use4 surf4
        usePC4 = (useableLandPC, combineTerryTables  (haPC) (snd use4) (snd pop4))
        mdRegion4 = wrapMdCol3 (reg4tot ++ [usePerc4, usePC4]) :: [MdColumn RegionId Double]

        mdRegion = markdownTable  regionNames2 regionOrder2 mdRegion4

    putStrLn mdRegion
    -- writeTab1Table "tab11" mdRegion

haPC a b = a * 10000 / b

-- fertilityPperyear conn = do
-- -- | compute an exensive indicator for fertility
-- --   multiply with number of woman (replace with 1/2 pop )
-- --   and assume that woman fertility is distributed over 20 years
--     let req = [population, fertilityRate]
--         years = map Year [2024,  2024, 2024]
--         reqYears = zip req years
--     c3@[pop3, fertRate3] :: [CountryTable3]
--     let women = scaleRegionTable (0.5*20) pops
--         fertilityCount3  = (fertilityCount combineTerryTables Multiply (snd women) (snd fertRate3))
--     return (fertilityCount3)

getData12 :: IO ()
-- fig12 -- bevoelkerung, wachstum migration
getData12 = do
    conn <- open dbPath

    pop3  <- lookupCountryTable3 conn (population)(Year 2024)
    popGrowth3 <- lookupCountryTable3 conn ( population) (Year 2023)
    fertRate3 <- lookupCountryTable3 conn fertilityRate (Year 2024)
    mignet3 <- lookupCountryTable3 conn migrationNet (Year 2024)
    close conn

    let c3 :: [CountryTable3]
        c3 = [pop3, popGrowth3, fertRate3, mignet3]

    -- test data availability
    let mcountry3 = wrapMdCol3 c3
        mdCountry = markdownTable allCodeNames xcountries mcountry3  --less1m mdC
    putStrLn mdCountry
    
    
    --olf     conn <- open dbPath
    -- let req = [population, populationGrowthRate, fertilityRate, migrationNet]
    --     years = map Year [2024, 2024, 2024, 2024]
    --     reqYears = zip req years

    -- c3@ :: [CountryTable3]
    --         <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
    -- close conn

    let -- women3 = second (scaleRegionTable (0.5*20)) pop3
        -- fertilityCount3  = (fertilityCount, combineTerryTables (*) (snd pop3) (snd fertRate3))
        popGrowthCount3  = (popGrowthCount, combineTerryTables (\a b -> a * b ) (snd pop3) (snd fertRate3))
        netmigPMP3 = (netMigrationCount, combineTerryTables (\a b -> (10**6) * a / b ) (snd mignet3) (snd pop3) ) -- :: RegionTable3
        c4 :: [CountryTable3]
        c4 = c3 ++ [ popGrowthCount3, netmigPMP3 ]
    -- -- let fertility2 = zip regionsList fertility

    -- putStrLn $ show popGrowthCount3
    let mcountry4 = wrapMdCol3 c4
        mdCountry = markdownTable allCodeNames xcountries mcountry4  --less1m mdC
    putStrLn mdCountry


    let reg4  :: [(Dataset, [(RegionId, TerryTable CountryId (WObs Double))])] 
        reg4 = reg3CountryTable4 regionMembers2 c4
        [pop4, popGrowth4, fertRate4, mignet4, popGrowthCount4, netmigPM4]
            = reg4tot  
        reg4tot  =  regtab3_regtab1 reg4 
        reg4tot :: [(Dataset, [TerryValue RegionId Double])]

        fertRate4x = combinesCountryTable3 fertilityRate (/) popGrowthCount4 pop4
        mdRegion4 = wrapMdCol3 (reg4tot ++ [fertRate4x] ) :: [MdColumn RegionId Double]

        mdRegion = markdownTable  regionNames2 regionOrder2 mdRegion4

    putStrLn mdRegion
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
    mObs <- latestObservation conn (CountryId "AUT") (dsIndicator surfaceArea)
    case mObs of
        Nothing ->
            print "nothing found"
            --assertFailure "No surface area found"

        Just obs ->
            print  $ obsYear obs
            -- @?= Year 2023

regionsWithExtra :: [RegionId]
regionsWithExtra = map fst regionMembers

dreiCountr = map CountryId ["FIN", "CYP", "PRT"]
xcountries = map CountryId ["FIN", "AUT", "BRA", "USA", "RUS"]

