-----------------------------------------------------------------------------
--
-- Module      :   Tables for test   -- used in countrySpec test !
-- for each region:
-- the population, the surface, gnp .
-- compare gnpPC with gnp/pop
-- within the region: standard dev. for surface per person

-----------------------------------------------------------------------------

module BaseTest.Tab99
where

import R4C.Model

-- import qualified Data.Text as T
import Database.SQLite.Simple -- for debug
import R4C.Export.Table
import R4C.Import.Query

-- import GHC.IO.Handle.Types (Handle__)
-- import GHC.Generics (Generic1(to1))
import Eins.Config
import Eins.Descriptor

-- import R4C.Statistics

import Eins.Region
import qualified Eins.Region as RBT
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- import R4C.Import.Database

import qualified Eins.Region3 as R3
import R4C.Export.CountnryCodeNames
import R4C.Pak
import R4C.Territory

-- import UniformBase
import Eins.Region2
import R4C.Country (lookupCountries)

exp1 = do
    _ <- exp1a less1m
    return ()
exp1t = exp1a threeCountries

exp1a :: [CountryId] -> IO String
-- countries is what is included in printed list
-- the gdp should be averaged ok automatically
exp1a countries = do
    conn <- open dbPath
    let req = [population, gdpPPpc, surfaceArea] -- gnpPPpc is not extensional
        years = map Year [2024, 2021, 2023]
        reqYears = zip (req) years -- :: [(Dataset, Year)]
    countryTables :: [CountryPak3] <-
        mapM (\(d, y) -> lookupCountryTable3 conn (d) y) reqYears
    close conn

    let mdC = wrapMdCol3 countryTables
    -- the operations on the tables must be with the mdcol data!
    let md = markdownTable allCodeNames countries mdC -- less1m mdC
    putStrLn md
    print md
    return (md)

threeCountries = [CountryId "MAF", CountryId "PLW", CountryId "NRU", CountryId "TUV"]
less1m = map CountryId R3.less1mTax

avcountries = map CountryId ["FIN", "CYP", "PRT", "AUT", "BRA", "BGD", "RUS"]
avregionOrder2 = take 4 $ map terryId regionNames2

exp3 :: IO ()
-- checks weighted average for gdpPC
exp3 = do
    _ <-
        exp3a
            regionOrder
            (map CountryId ["FIN", "CYP", "USA", "AUT", "BRA", "BGD", "RUS"])
    return ()

-- exp3 :: IO ()

{- | show all countries with popuplation surface and GNP
exp3a :: [RegionId] -> [CountryId] -> IO (String, String)
regOrder and countries list what is include in result
-}
exp3a :: [RegionId] -> p -> IO String
exp3a regOrder countries = do
    let regionDef = RBT.regionMembers -- g7, eu, russia
    conn <- open dbPath
    let reqYears =
            [ (population, Year 2024)
            , (gnp, Year 2021)
            , (surfaceArea, Year 2023)
            , (gdpPPpc, Year 2021) -- intensional, weighted by population
            ]
    [pop3, surf3, gdp3, gnp3] <-
        mapM (uncurry $ lookupCountryTable3 conn) reqYears
    -- pop3  <- lookupCountryTable3 conn (population)(Year 2024)
    -- surf3 <- lookupCountryTable3 conn ( surfaceArea) (Year 2023)
    -- gdp3 <- lookupCountryTable3 conn gdpPPpc (Year 2023)
    -- gnp3 <- lookupCountryTable3 conn gnp (Year 2021)
    -- close conn

    let c3 = [pop3, surf3, gdp3, gnp3]
    let c4 = c3
    -- type RegionTable3 = (Dataset, [(RegionId, CountryTable)]) -- new format

    let
        reg4 :: [(Dataset, [(RegionId, TerryTable CountryId (WObs Double))])]
        reg4 = reg3CountryTable4 regionMembers2 c4
        reg4tot :: [(Dataset, [TerryValue RegionId Double])]
        reg4tot = regtab3_regtab1 reg4

        mdRegion4 = wrapMdCol3 reg4tot
        mdRegion = markdownTable regionNames2 avregionOrder2 mdRegion4
    putStrLn mdRegion
    -- print mdRegion
    return mdRegion

-- mdC :: [MdColumn RegionId Double]
--     mdC = wrapMdCol3 c4
-- let md1 = markdownTable RBT.regionNames regOrder mdC
-- putStrLn md1
-- print md1
-- return md1

exp4 :: IO ()
exp4 = do
    _ <-
        exp3b
            regionOrder
            (map CountryId ["FIN", "CYP", "USA", "AUT", "BRA", "BGD", "RUS"])
    return ()

exp3b :: [RegionId] -> [CountryId] -> IO String
exp3b regOrder countries = do
    let regionDef = RBT.regionMembers -- g7, eu, russia
    conn <- open dbPath
    let reqYears =
            [ (population, Year 2024)
            , (surfaceArea, Year 2023)
            , (gnp, Year 2021)
            , (gdpPPpc, Year 2021)
            ]
    [pop3, surf3, gnp3, gdp3] <-
        mapM (uncurry $ lookupCountryTable3 conn) reqYears

    -- pop3  <- lookupCountryTable3 conn (population)(Year 2024)
    -- surf3 <- lookupCountryTable3 conn ( surfaceArea) (Year 2023)
    -- gdp3 <- lookupCountryTable3 conn gdpPPpc (Year 2023)
    -- gnp3 <- lookupCountryTable3 conn gnp (Year 2021)
    close conn

    let c3 = [pop3, surf3, gdp3, gnp3]

        mdC = wrapMdCol3 c3
    let md1 = markdownTable allCodeNames countries mdC
    putStrLn md1
    -- print md1
    return md1

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
