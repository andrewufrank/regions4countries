-----------------------------------------------------------------------------
--
-- Module      :   experiments with Terry data  for counries 
-- especially small countries 

-----------------------------------------------------------------------------

module R4C.CountryExperiments
    where

import R4C.Model
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator 
import Study.Region2 
import R4C.Aggregate 
import R4C.Import.Query
import GHC.IO.Handle.Types (Handle__)
import GHC.Generics (Generic1(to1))
import Study.Config 
import Study.Dataset 
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UniformBase hiding ((</>))
import R4C.Import.Database
-- import R4C.Export.CountryTable
import R4C.Export.Table 
import R4C.Export.CountnryCodeNames
import R4C.Region3 




-- exp2 :: IO ()
-- -- } show the totals for the small, and very small countries
-- exp2 = do 
--     conn <- open dbPath 


--     tabPop :: CountryTable  <- lookupTable conn (dsIndicator population)(Year 2024)
--     tabGNP :: CountryTable <- lookupTable conn (dsIndicator grossNatProd) (Year 2024)
--     tabSurf :: CountryTable <- lookupTable conn (dsIndicator surfaceArea) (Year 2023)
--     -- for weighted values aggregateTable does not work
--     -- tabGNPpcwb <- lookupTable conn (dsIndicator gnpPPpc) (Year 2024)
--     -- gnpPCwb <-  (aggregate regionMembersSmall conn gnpPPpc (Year 2024))  

--     let 
--         pops = aggregateTable (aggregationFunction $ dsAggregation population) regionMembersSmall  tabPop
--         surfs = aggregateTable (aggregationFunction $ dsAggregation surfaceArea) regionMembersSmall tabSurf 
--         gnp = aggregateTable (aggregationFunction $ dsAggregation grossNatProd) regionMembersSmall tabGNP
--         -- gnpPCwb =  aggregateTable (aggregationFunction $ dsAggregation gnpPPpc) regionMembersSmall tabGNPpcwb

--         gnpPC = combineRegionTables Divide gnp pops
    
--     close conn

--     let mdCols = 
--             [ pops, surfs, gnp, gnpPC
--             -- , gnpcWB 
            
--             -- MdColumn "Bevoelkerung 2024 (Mega)" Mega 0  pops
--             -- , MdColumn "Flaeche 2023 (Mega km²)" Mega 2  surfs
--             -- , MdColumn "GNP (Mega $)" Giga 0 gnp
--             -- , MdColumn "GNP pc (Kilo $)" Kilo 0 gnpPC  
--             -- , MdColumn "GNP pc WorldBank cal (Kilo $)" Kilo 0 gnpPCwb 
--             --         Centi 1 surfPerCap
--             -- , MdColumn "Nutzbares Land per capita (ha/person)" 
--             --         Centi 0 surAgrarfPerCap 
--             ]
--     let sortedRegions = sortTerryByColumn Descending  pops  

--     -- let md = markdownTable regionsList mdCols
--     let md = markdownTable regionNames2 sortedRegions mdCols
--     putStrLn md 
--     writeTab1Table "tab11" md


exp1 :: IO ()  
-- | show all countries with popuplation surface and GNP
-- fig11 
exp1 = do
    conn <- open dbPath 
    let req = [population, gnpPPpc, surfaceArea]
        years = map Year [2024, 2024, 2023]
        reqYears = zip (map dsIndicator req) years-- :: [(Dataset, Year)]
    countryTables <- mapM (\(d,y) -> lookupTable conn ( d) y) reqYears
    -- tabPop :: CountryTable  <- lookupTable conn (dsIndicator population)(Year 2024)
    -- tabGNPpc :: CountryTable <- lookupTable conn (dsIndicator gnpPPpc) (Year 2024)
    -- tabSurf <- lookupTable conn (dsIndicator surfaceArea) (Year 2023)

    close conn

    
    -- let mdCols = 
    --         [ MdColumn "Bevoelkerung 2024 (Mega)" Mega 6  tabPop
    --         , MdColumn "Flaeche 2023 (Kilo km²)" Kilo 0  tabSurf
    --         , MdColumn "GNP pro Kopf (kilo PP)" Kilo 0 tabGNPpc 
             
    --         ]
    -- let  less1m = map CountryId less1Mcountries :: [CountryId]
    -- let sort = sortTerryByColumn Descending  tabGNPpc --surfPerCap
    -- let sort2 = filter (\c -> elem c less1m) sort
    let  mdC = map (\(t,d) -> wrapMdCol d t) $ zip countryTables req

    let md = markdownTable allCodeNames less1m mdC
    putStrLn md 
    -- putStrLn . show . map unCountryId $ less1m
    -- writeTab1Table "exp1" md
    return ()

-- wrapMdCol :: TerryTable t v -> MdColumn t v
wrapMdCol dataset ct = MdColumn {colTitle =   t2s $ dsShortName dataset 
                    , colScale = dsScale dataset
                    , colUnit =  dsUnit dataset 
                    , colDecimals =  dsDecimals dataset
                    , colValues = ct}
less1m = map CountryId less1Mcountries

-- break was 60k$ GNP 2024

-- regionMembersSmall :: RegionMembers  -- [(RegionId, CountryId)]
-- regionMembersSmall = 
--   [ mk "less10mio" less10Mcountries 
--     , mk "less1mio" less1Mcountries
--   ]
--   where
--     mk :: Text -> [Text] -> (RegionId, [CountryId])
--     mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)

-- tableOutputDirectoryExp :: FilePath
-- tableOutputDirectoryExp =   "/home/frank/CountryExperiments"

-- writeTab1Table :: FilePath -> String -> IO ()
-- writeTab1Table filename contents = do
--     createDirectoryIfMissing True tableOutputDirectoryExp
--     writeFile (  tableOutputDirectoryExp </> filename) contents
