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
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
import R4C.Import.Query
import R4C.Export.Table 
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 
import Study.Descriptor
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import BaseTest.Region
import qualified BaseTest.Region as RBT 
import R4C.Import.Database 
import R4C.Territory 
import R4C.Export.CountnryCodeNames
import qualified R4C.Region3 as R3 
import R4C.Pak
import UniformBase 

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

exp1 = do 
    _ <- exp1a less1m 
    return ()
exp1t = exp1a threeCountries

exp1a :: [CountryId] -> IO String
-- countries is what is included in printed list 
exp1a countries= do
    conn <- open dbPath 
    let req = [population, gdpPPpc, surfaceArea]  -- gnpPPpc is not extensional 
        years = map Year [2024, 2021, 2023]
        reqYears = zip ( req) years-- :: [(Dataset, Year)]
    countryTables :: [CountryTable3] <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
    close conn

    let mdC = wrapMdCol3 countryTables
-- the operations on the tables must be with the mdcol data! 
    let md = markdownTable allCodeNames countries mdC  --less1m mdC
    putStrLn md 
    print md
    return (md)

threeCountries = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
less1m = map CountryId R3.less1mTax


exp3 :: IO ()
exp3 = do 
    _ <- exp3b regionOrder  (map CountryId ["FIN", "CYP", "PRT"])  
    return ()

-- exp3 :: IO ()  
-- | show all countries with popuplation surface and GNP
-- exp3a :: [RegionId] -> [CountryId] -> IO (String, String)
-- regOrder and countries list what is include in result
exp3a :: [RegionId] -> p -> IO String
exp3a regOrder countries = do
    let regionDef = RBT.regionMembers -- g7, eu, russia 

    conn <- open dbPath 
    let reqYears = [(population, Year 2024), (gnp, Year 2021), (surfaceArea, Year 2023), (gdpPPpc, Year 2021)]
    regionCountryTables :: [RegionTable3]  <- mapM (\(d,y) -> lookupRegionTable3 conn regionDef d y) reqYears
    close conn

    let mdC :: [MdColumn RegionId Double]
        mdC = wrapMdCol3 . regtab3_regtab1 $ regionCountryTables 
    let md1 = markdownTable RBT.regionNames regOrder mdC
    putStrLn md1 
    print md1
    return md1

exp3b :: [RegionId] -> [CountryId] -> IO String
exp3b regOrder countries = do
    let regionDef = RBT.regionMembers -- g7, eu, russia 

    conn <- open dbPath 
    let reqYears = [(population, Year 2024), (gnp, Year 2021), (surfaceArea, Year 2023), (gdpPPpc, Year 2021)]
    regionCountryTables :: [RegionTable3]  <- mapM (\(d,y) -> lookupRegionTable3 conn regionDef d y) reqYears
    close conn

    let mdC :: [MdColumn RegionId Double]
        mdC = wrapMdCol3 . regtab3_regtab1 $ regionCountryTables 
    let md1 = markdownTable RBT.regionNames regOrder mdC
    putStrLn md1 

    -- get OneCountry 
    let regid = RegionId "EU"
    let euMdC = getOneRegionMany3  regionCountryTables regid
    let md2 =  (markdownTable allCodeNames countries) $   euMdC --less1m mdC
    putStrLn  md2 
    print md1 
    print md2
    return md2


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
    mObs <- latestObservation conn (CountryId "AUT") (IndicatorId "NY.GDP.PCAP.PP.CD")
    case mObs of
        Nothing ->
            print "nothing found" 
            --assertFailure "No surface area found"

        Just obs ->
            print  $ obsYear obs 
            -- @?= Year 2023


testObs :: IO () 
testObs = do 
    conn <- open dbPath
    mObs <- allObservation conn (CountryId "AUT") (Year 2023)  --- IndicatorId "NY.GDP.PCAP.PP.CD")
    case mObs of
        Nothing ->
            print "nothing found" 
            --assertFailure "No surface area found"

        Just obs ->
              print . map obsIndicator  $   obs 
            -- @?= Year 2023
