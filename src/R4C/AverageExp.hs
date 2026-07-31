-----------------------------------------------------------------------------
--
-- Module      :   experiments with Terry data 
-- especially small countries 
-- but all on countries 

-----------------------------------------------------------------------------

module R4C.AverageExp
    where

import R4C.Model
import qualified Data.Text as T
import Database.SQLite.Simple  -- for debug
-- import Study.Indicator 
import Study.Region2 
import R4C.Aggregate 
import R4C.Import.Query
import GHC.IO.Handle.Types (Handle__ (Handle__))
import GHC.Generics (Generic1(to1))
import Study.Config 
-- import Study.Dataset 
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
import R4C.Pak 
import Study.Descriptor 
-- import Study.Tab1  
import R4C.Territory ( regtab3_regtab1, wrapMdCol3 )  
import BaseTest.Region  

regionMembersSmall :: RegionMembers  -- [(RegionId, CountryId)]
regionMembersSmall = 
  [ mk "less10mio" less10Mcountries 
    , mk "less1mio" less1Mcountries
  ]
  where
    mk :: Text -> [Text] -> (RegionId, [CountryId])
    mk r cs = (RegionId r, map (\c -> (CountryId c)) cs)

avcountries = map CountryId ["FIN", "CYP", "PRT"] -- "AUT", "BRA", "BGD", "RUS"]
avregionOrder2 = take 2 $ map terryId regionNames2 




exp1 :: IO ()  
-- | show all countries with popuplation surface and GNP
-- fig11 
exp1 = do
    conn <- open dbPath 

    pop3  <- lookupCountryTable3 conn (population)(Year 2024)
    surf3 <- lookupCountryTable3 conn ( surfaceArea) (Year 2023)
    fert3 <- lookupCountryTable3 conn fertilityRate (Year 2023)

    close conn

    let c3 = [pop3, surf3, fert3]
        c4 = c3 :: [(Dataset, [TerryValue CountryId (WObs Double)])]

        mCountry4 = wrapMdCol3 c4 
        mdCountry = markdownTable allCodeNames avcountries mCountry4
    putStrLn mdCountry 

    let reg4:: [(Dataset, [(RegionId, TerryTable CountryId (WObs Double))])] 
        reg4  = reg3CountryTable4 regionMembers2 c4 
        reg4tot :: [(Dataset, [TerryValue RegionId Double])]
        reg4tot = regtab3_regtab1 reg4  

        mdRegion4 = wrapMdCol3 reg4tot
        mdRegion = markdownTable regionNames2 avregionOrder2  mdRegion4 
    putStrLn mdRegion 


    -- -- let mdCols = 
    -- --         [ MdColumn "Bevoelkerung 2024 (Mega)" Mega 6  tabPop
    -- --         , MdColumn "Flaeche 2023 (Kilo km²)" Kilo 0  tabSurf
    -- --         , MdColumn "GNP pro Kopf (kilo PP)" Kilo 0 tabGNPpc 
             
    -- --         ]
    -- -- let  less1m = map CountryId less1Mcountries :: [CountryId]
    -- let sort = sortTerryByColumn Descending  tabGNPpc --surfPerCap
    -- let sort2 = filter (\c -> elem c less1m) sort

    -- let md = markdownTable allCodeNames sort2 [tabPop, tabGNP, tabGNPpc, tabSurf]
    -- putStrLn md 
    -- putStrLn . show . map unCountryId $ sort2
    -- writeTab1Table "exp1" md
    -- return ()

less1m = map CountryId less1Mcountries

-- break was 60k$ GNP 2024
