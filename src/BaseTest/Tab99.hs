-----------------------------------------------------------------------------
--
-- Module      :   Tables for test 
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

exp1a countries= do
    -- let countries = less1m  -- countries included 
    conn <- open dbPath 
    let req = [population, gdpPPpc, surfaceArea]  -- gnpPPpc is not extensional 
        years = map Year [2024, 2021, 2023]
        reqYears = zip ( req) years-- :: [(Dataset, Year)]
    countryTables :: [CountryTable3] <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
    close conn

    -- let aggs = map (\a -> map (aggregateTery a countries) countryTables) [sum1, min1, median1, max1, mean1, stdDev1] 
    -- putIOwords ["the sums are", showT aggs]

    -- let  mdC = map (\(t,d) -> wrapMdCol d t) $ zip countryTables req :: [MdColumn CountryId Double]
    let mdC = wrapMdCol3 countryTables
-- the operations on the tables must be with the mdcol data! 
    -- let sortedRegions = sortTerryByColumn Descending  (headNote "wewer" countryTables) 
    let md = markdownTable allCodeNames countries mdC  --less1m mdC
    putStrLn md 
    -- putStrLn . show . map unCountryId $ less1m
    -- writeTab1Table "exp1" md
    -- putStrLn . show $ sortedRegions
    return (md)

x1 = do 
    conn <- open dbPath 
    t <- lookupTable conn (IndicatorId "NY.GNP.MKTP.PP.KD") (Year 2023)
    putIOwords [showT t]
x2 = do 
    conn <- open dbPath 
    t <- lookupTable conn (IndicatorId "NY.GDP.PCAP.PP.CD") (Year 2023)
    putIOwords [showT t]

threeCountries = [CountryId "MAF",CountryId "PLW",CountryId "NRU",CountryId "TUV"]
less1m = map CountryId R3.less1mTax


exp3 = do 
    _ <- exp3a regionOrder   threeCountries  
    return ()
-- exp3 :: IO ()  
-- | show all countries with popuplation surface and GNP
exp3a :: [RegionId] -> [CountryId] -> IO (String, String)
exp3a regOrder countriesOrder = do
    let
        countries = less1m  -- countries included 
        regionDef = RBT.regionMembers -- g7, eu, russia 

    conn <- open dbPath 
    let req = [population, gnpPPpc, surfaceArea]
        years = map Year [2024, 2024, 2023]
        reqYears = zip ( req) years-- :: [(Dataset, Year)]
    regionCountryTables :: [RegionTable3]  <- mapM (\(d,y) -> lookupRegionTable3 conn regionDef d y) reqYears
    close conn

    let aggs = map (\a -> map (aggregateTerry3 a ) regionCountryTables) [min1, median1, max1, mean1, stdDev1] 
    putIOwords ["the sums are", showT aggs]
    
    -- convert to regionTable 
    -- make a single val for each country 
    let rct = regionCountryTables :: [RegionTable3] -- (Dataset, [(RegionId, CountryTable)]) 
        rct2  :: [(Dataset, [TerryValue RegionId Double ])]
        rct2 = map xone rct
        xone :: (Dataset, [(RegionId, CountryTable)]) -> (Dataset, [TerryValue RegionId Double ]) 
        xone (ds, tab) = (ds,  sumCountryTables tab)

    let  mdC = wrapMdCol3 rct2
    -- let  mdC = map (\(t,d) -> wrapMdCol d t) t2 req
-- the operations on the tables must be with the mdcol data! 

    let md1 = markdownTable RBT.regionNames regOrder mdC
    putStrLn md1 

    -- get OneCountry 
    let regid = RegionId "EU"
    -- let euMdC = catMaybes $ zipWith (\pop tab -> getOneRegion regid pop  tab) req rct  :: [MdColumn CountryId Double]
    let euMdC = getOneRegionMany3  rct regid
    let md2 =  (markdownTable allCodeNames countriesOrder) $   euMdC --less1m mdC
    putStrLn  md2 
    return (md1, md2)



-- usableAreaPerCapita1 :: Connection -> IO (MdColumn RegionId Double)
-- -- | a virtual dataset for the useful area ha per capita (per region)
-- usableAreaPerCapita1 conn = do 
--     pops <-  (aggregate regionMembers conn population (Year 2024)) 
--     arabl <-  (aggregate regionMembers conn arableLand (Year 2023))  -- add pastures, forest, urban 
--             -- ersetzt durch agrarlandPart

--     let surfPerCap = combineRegionTables Divide arabl pops
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
--     -- let surfPerCap = combineRegionTables Divide surfs3 pops3
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
--         fertilityCount  = combineRegionTables Multiply women fertility 
--     return (fertilityCount)

-- agrarlandPC :: Connection -> IO (MdColumn RegionId Double)
-- issue with weighted
-- agrarlandPC conn = do 
-- -- agrarland (arable, permant crops or pasture) per capita
--     pops <- aggregate regionMembers conn population (Year 2024)
--     surf <- aggregate regionMembers conn surfaceArea (Year 2023)
--     agrarPart <-  (aggregate regionMembers conn agrarlandPart (Year 2023))  -- non-extensional
--     let agrarPerc = scaleRegionTable (0.01) agrarPart  -- convert % to factor
--         agrarTotal = combineRegionTables Multiply agrarPerc surf
--         agrarsurfPerCap = combineRegionTables Divide agrarTotal pops
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
