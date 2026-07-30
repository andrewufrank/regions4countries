-- {-# ANN myFunction ("HLint: ignore Use fewer guards" :: String) #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-----------------------------------------------------------------------------
--
-- Module      :   experiments which show difference gnp gdp 
-- which is the income from int. prop. rights (licences) 
-- especially small countries 

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Study.Tab7gdp_gnp     
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
import Study.Descriptor
import R4C.Statistics
import R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UniformBase hiding ((</>))
import R4C.Import.Database
-- import R4C.Export.CountryTable
import R4C.Export.Table 
import R4C.Export.CountnryCodeNames
import qualified R4C.Region3 as R3 
import qualified BaseTest.Region as RBT 
import BaseTest.Region (regionOrder)
import Data.List
import R4C.Territory
import R4C.Pak 

exp5 = do   -- get regions from exp1a
    _ <- exp5a euCountries RBT.regionMembers  -- EU, G7
    return ()

-- exp5 :: [CountryId] -> IO String
-- countries is what is included in printed list 
exp5a :: [CountryId] -> [(RegionId, [CountryId])] -> IO String
exp5a countries regionDef = do
    conn <- open dbPath 
    let  reqYears =  [(population, Year 2024), (gnpPP, Year 2021),  (gdpPPpc, Year 2021)]

    countryTables :: [CountryTable3] <- mapM (\(d,y) -> lookupCountryTable3 conn ( d) y) reqYears
    close conn
    let mdC = wrapMdCol3 countryTables
    let [pop3, g3, gpc3] = mdC :: [MdColumn CountryId Double]

    let gpc4 = (combineMdTables Divide g3  pop3){colScale=Kilo}
        diff4 = (combineMdTables (Subtract) gpc3 gpc4)
        mdC4 = mdC ++ [gpc4, diff4]  -- the countries with difference between GDP and GNP (cyprus, ireland, luxemburg, malta)
-- the operations on the tables must be with the mdcol data! 
    let md = markdownTable allCodeNames countries mdC4  --less1m mdC
    putStrLn md 

    -- now for the regions
    let [popt, gt, gpct]= countryTables
        gt:: (Dataset, TerryTable CountryId Double) 
        gnct = (ds1, combineTerryTables (/) (snd gt) (snd popt)) 
        difft = (ds2, combineTerryTables (-) (snd gnct) (snd gpct))
        ds1 = Dataset{dsName = "gnpPP per cap"
                    , dsShortName ="gnpPP per cap"
                    , dsScale = Kilo
                    , dsUnit = "PP$/P"
                    , dsDecimals = 0 }
        ds2 = Dataset{dsName = "diff"
                    , dsShortName ="diff"
                    , dsScale = Kilo
                    , dsUnit = "PP$/P"
                    , dsDecimals = 0 }        -- gpc4 = MdColumn { colValues = combineTerryTables (/) (snd gt) (snd popt) 
        --                 , colTitle = "gnpPP per cap"
        --                 , colScale = Kilo
        --                 , colUnit = "PP$"
        --                 , colDecimals = 0
        --                 }   

        -- gt4 = [pop3, g3, gpc3] ++ [gpc4]
        -- -- reg3CountryTable4 regionDef mdC
        gt4 = countryTables ++ [gnct, difft] :: [CountryTable3]
        reg4:: [(Dataset, [(RegionId, TerryTable CountryId Double)])]
        reg4 = reg3CountryTable4 regionDef gt4 :: [(Dataset, [(RegionId, TerryTable CountryId Double)])]
        reg4tot =  regtab3_regtab1 reg4 :: [(Dataset, [TerryValue RegionId Double])]
        -- reg4tot = map (second sumCountryTables) reg4 
        md4 = wrapMdCol3 reg4tot :: [MdColumn RegionId Double]
    let md2 = markdownTable RBT.regionNames RBT.regionOrder md4  --less1m mdC
    putStrLn md2 

    print md
    return (md)
