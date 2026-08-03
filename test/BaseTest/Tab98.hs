-----------------------------------------------------------------------------
--
-- Module      :   Tables for test   -- used in countrySpec test !
-- for each region:
-- the population, the surface, gnp .
-- compare gnpPC with gnp/pop
-- within the region: standard dev. for surface per person
-- here try convert to extensive - for gdpPC
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BaseTest.Tab98 where

-- import qualified Data.Text as T
import Database.SQLite.Simple -- for debug

-- import GHC.IO.Handle.Types (Handle__)
-- import GHC.Generics (Generic1(to1))
import Eins.Config
import Eins.Descriptor
import Eins.Descriptor2

-- import R4C.Statistics

import Eins.Region
import qualified Eins.Region as RBT

-- import R4C.Import.Database

-- import UniformBase
import Eins.Region2
import qualified Eins.Region3 as R3
import R4C.Country (lookupCountries)
import R4C.Export.CountnryCodeNames
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import R4C.Export.Table
import R4C.Import.Query
import R4C.Model
import R4C.Pak
import R4C.Territory
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))


exp5 :: IO ()
-- | check the conversion to extensive for gdp per cap, compare with gnp....
-- test regions 
exp5 = do
    _ <- exp5a (map RegionId ["EU", "RUSSIA"])   
    return ()

-- exp5 :: IO ()
-- | show all countries with popuplation surface and GNP
-- exp5a :: [RegionId] -> [CountryId] -> IO (String, String)
-- regOrder and countries list what is include in result
exp5a :: [RegionId]   -> IO String
exp5a regOrder   = do
    let regionDef = RBT.regionMembers -- g7, eu, russia

    conn <- open dbPath
    let reqYears =
            [ (population, Year 2024)
            , (surfaceArea, Year 2023)
            , (gnpPP, Year 2021)
            , (gdpPPpc, Year 2021) -- per capita, intensive
            ]
    c3 <- mapM (uncurry $ lookupCountryTable3 conn) reqYears
    let [pop3, surf3, gdp3, gnpPc3] = c3

    let gdp5 = makePakExtensive gnpPc3 (Year 2021)

    close conn

    let c4 = c3 ++ [gdp5]
        reg4tot :: [RegionPak3]
        reg4tot = country2regionPak regionMembers2 c4

        mdRegion4 = map wrapMdCol1 reg4tot
        mdRegion = markdownTable regionNames2 regOrder  mdRegion4
    putStrLn mdRegion
    -- print mdRegion
    return mdRegion

-- mdC :: [MdColumn RegionId Double]
--     mdC = map wrapMdCol1 c4
-- let md1 = markdownTable RBT.regionNames regOrder mdC
-- putStrLn md1
-- -- print md1
-- return md1


exp6 :: IO ()
-- | check the conversion to extensive for gdp per cap, compare with gnp....
-- test countries 
exp6 = do
    _ <-
        exp6b
            -- regionOrder
            (map CountryId ["FIN", "CYP", "PRT", "AUT", "BRA", "USA", "RUS"])
    return ()

exp6b ::  [CountryId] -> IO String
exp6b  countries = do
    let regionDef = RBT.regionMembers -- g7, eu, russia
    conn <- open dbPath
    let reqYears =
            [ (population, Year 2024)
            , (surfaceArea, Year 2023)
            , (gnpPP, Year 2021)
            , (gdpPPpc, Year 2021) -- per capita, intensive
            ]
    c3 <- mapM (uncurry $ lookupCountryTable3 conn) reqYears
    let [pop3, surf3, gdp3, gnpPc3] = c3

    let gdp5 = makePakExtensive gnpPc3 (Year 2021)

    close conn

    let c4 = c3 ++ [gdp5]
        mdC = map wrapMdCol1 c4
    let md1 = markdownTable allCodeNames countries mdC
    putStrLn md1
    -- print md1
    return md1

threeCountries =
    [CountryId "MAF", CountryId "PLW", CountryId "NRU", CountryId "TUV"]

less1m = map CountryId R3.less1mTax

avcountries = map CountryId ["FIN", "CYP", "PRT"] -- "AUT", "BRA", "BGD", "RUS"]

avregionOrder2 = take 6 $ map terryId regionNames2


-- writeTab1Table :: FilePath -> String -> IO ()
-- writeTab1Table filename contents = do
--     createDirectoryIfMissing True tableOutputDirectory
--     writeFile (tableOutputDirectory </> filename) contents

-- popsSurf :: p -> IO (RegionTable, RegionTable) -- ([(RegionId, Maybe Double)], [(RegionId, Maybe Double)])
-- popsSurf conn =  do
--     conn <- open dbPath
--     pops <-  (aggregate regionMembers conn population (Year 2024))
--     surfs <-  (aggregate regionMembers conn surfaceArea (Year 2023))

--     return (pops,surfs)

-- -- get OneCountry
-- let regid = RegionId "EU"
-- let euMdC = getOneRegionMany3  c3 regid
-- let md2 =  (markdownTable allCodeNames countries) $   euMdC --less1m mdC
-- putStrLn  md2
-- print md1
-- print md2
-- return md2

-- usableAreaPerCapita1 :: Connection -> IO (MdColumn RegionId Double)
-- -- | a virtual dataset for the useful area ha per capita (per region)
-- usableAreaPerCapita1 conn = do
--     pops <-  (aggregate regionMembers conn population (Year 2024))
--     arabl <-  (aggregate regionMembers conn arableLand (Year 2023))  -- add pastures, forest, urban
--             -- ersetzt durch agrarlandPart

--     let surfPerCap = combineMdTables Divide arabl pops
--     let surfpc2 = surfPerCap {colScale=Unit, colDecimals=6}  -- Mega/Mega
--     return surfpc2

-- getData12 :: IO ()
-- -- fig12
-- getData12 = do
--     conn <- open dbPath
--     (pops3, surfs3) <- popsSurf conn
--     -- surfpc2 <- surfacePerCapita conn
--     -- arablpc <- usableAreaPerCapita1 conn

--     close conn
--     -- let surfPerCap = combineMdTables Divide surfs3 pops3
--     --     -- surfPerCapM = scaleRegionTable (10**6) surfPerCap -- convert km2 to m2

--     -- let surfpc2 = surfPerCap {colScale=Unit, colDecimals=6}  -- Mega/Mega
--     let md = markdownTable regionNames regionOrder [pops3, surfs3, surfpc2, arablpc]
--     putStrLn md
--     writeTab1Table "tab12" md

-- fertilityPperyear conn = do
-- -- | compute an exensional indicator for fertility
-- --   multiply with number of woman (replace with 1/2 pop )
--     pops <-  (aggregate regionMembers conn population (Year 2024))
--     fertility <-  (aggregate regionMembers conn fertilityRate (Year 2024))
--     let women = scaleRegionTable (0.5) pops
--         fertilityCount  = combineMdTables Multiply women fertility
--     return (fertilityCount)

-- agrarlandPC :: Connection -> IO (MdColumn RegionId Double)
-- issue with weighted
-- agrarlandPC conn = do
-- -- agrarland (arable, permant crops or pasture) per capita
--     pops <- aggregate regionMembers conn population (Year 2024)
--     surf <- aggregate regionMembers conn surfaceArea (Year 2023)
--     agrarPart <-  (aggregate regionMembers conn agrarlandPart (Year 2023))  -- non-extensional
--     let agrarPerc = scaleRegionTable (0.01) agrarPart  -- convert % to factor
--         agrarTotal = combineMdTables Multiply agrarPerc surf
--         agrarsurfPerCap = combineMdTables Divide agrarTotal pops
--     let surfpc2 = agrarsurfPerCap {colScale=Unit, colDecimals=6}

--     return surfpc2

-- getData13 = do
--     conn <- open dbPath
--     (pops3, surfs3) <- popsSurf conn

--     -- usablePC <- usableAreaPerCapita conn
--     -- agrarPC <- agrarlandPC conn  -- TODO

--     -- netmigration <-  (aggregate regionMembers conn migrationNet (Year 2024))
--     -- fertilityPyear <-  fertilityPperyear conn
--     -- agrar <-  (aggregate regionMembers conn agrarland (Year 2023))    -- nur ackerland!
--     -- agrarPC <-  (aggregate regionMembers conn agriculturalLandPC (Year 2023))
--             -- wheigted!
--     close conn

-- let mdCols =
--         [ pops3
--         , surfs3
--         , usablePC
--         -- , agrarPC
--         ]
-- let md = markdownTable regionNames regionOrder mdCols
-- putStrLn md

-- storeTables :: IO ()
-- -- | Regenerate all Tab1 output files and update the book markdown files.
-- storeTables = do
--     getData11
--     -- getData12
--     let filename = buch </> "p99Tableaux" </> "099test.md"
--     let tables = ["tab11", "tab12" ]
--     mapM_ (\tab -> writeMarkdownBlock filename filename tab (tableOutputDirectory </> tab)) tables

-- move later somewhere
testlatest :: IO ()
testlatest = do
    conn <- open dbPath
    mObs <-
        latestObservation
            conn
            (CountryId "AUT")
            (IndicatorId "NY.GDP.PCAP.PP.CD")
    case mObs of
        Nothing ->
            print "nothing found"
        -- assertFailure "No surface area found"

        Just obs ->
            print $ obsYear obs

-- @?= Year 2023

testObs :: IO ()
testObs = do
    conn <- open dbPath
    mObs <- allObservation conn (CountryId "AUT") (Year 2023) --- IndicatorId "NY.GDP.PCAP.PP.CD")
    case mObs of
        Nothing ->
            print "nothing found"
        -- assertFailure "No surface area found"

        Just obs ->
            print . map obsIndicator $ obs

-- @?= Year 2023
