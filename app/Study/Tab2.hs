-----------------------------------------------------------------------------
--
-- Module      :   Tab2 ernaehrung
-- for each region:
-- the population, the surface, surface per person,
-- within the region: standard dev. for surface per person

-----------------------------------------------------------------------------

module Tab2
where

import Database.SQLite.Simple
import Eins.Config
import Eins.Descriptor
import Eins.Region (regionOrder)
import Eins.Region2
import Eins.Region3 hiding (regionOrder)
import R4C.Export.CountnryCodeNames
import R4C.Export.Table
import R4C.Import.Database
import R4C.Import.Query
import R4C.Model
import R4C.Pak
import R4C.Territory
import R4C.TerryTable
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UniformBase hiding (uncurry, (</>))

getData21 :: IO ()
-- fig21 cereal production and veg. consumption
getData21 = do
    conn <- open dbPath
    pop3 <- lookupCountryPak conn (population) (Year 2024)
    cerealProd <- lookupCountryPak conn cerealProduction (Year 2023) -- 2024 not all values
    -- AG.PRD.CREL.MT
    let cerealFood3 = (scalePak 0.1 pop3){pDataSet = cerealFood} -- 100 kg per head
    close conn

    let regionPaks =
            countryToRegionPaks regionMembers2 [cerealProd, cerealFood3] ::
                [RegionPak3]
        mdCols = map wrapMdCol1 regionPaks
    --  MdColumn "Getreideproduktion (T kg)" Mega 2  cerealProd
    -- -- value is t
    -- , MdColumn "menschliche Ernaehrung (Mega kg)" Mega 2  cerealFood
    -- value is 10**11 kg
    -- let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 regionOrder mdCols
    putStrLn md
    writeTab2Table "tab21" md

getData22 :: IO ()

-- | Ernaehrungssituation 1980 (ohne Russland, noch nicht existent)
getData22 = do
    conn <- open dbPath
    pops1980 <- lookupCountryPak conn population (Year 1980)
    cerealProd <- lookupCountryPak conn cerealProduction (Year 1980)
    let cerealFood3 = (scalePak 0.1 pops1980){pDataSet = cerealFood}
        cerealDomUse3 = (scalePak 2.5 cerealFood3){pDataSet = cerealDomesticUse}
        potExport3 =
            (combinePaks Subtract cerealProd cerealDomUse3)
                { pDataSet = potentialCerealExport
                }

    close conn

    let regionPaks =
            countryToRegionPaks
                regionMembers2
                [cerealProd, cerealFood3, cerealDomUse3, potExport3] ::
                [RegionPak3]
        mdCols = map wrapMdCol1 regionPaks
    --  MdColumn "Getreideproduktion (T kg)" Mega 0  cerealProd
    -- -- value is t
    -- , MdColumn "menschliche Ernaehrung (Mega kg)" Mega 0  cerealFood
    -- -- value is 10**11 kg
    -- , MdColumn "total Verbrauch (Mega kg)" Mega 0 cerealDomUse
    -- , MdColumn "potential fuer Export (Mega kg)" Mega 0 potExport

    -- let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 regionOrder mdCols
    putStrLn md
    writeTab2Table "tab22" md

-- duengerverbrauch und produktion
getData23 = do
    conn <- open dbPath
    pops3 <- lookupCountryPak conn population (Year 2023)
    arablePC3 <- lookupCountryPak conn arableLandPC (Year 2023)
    fertConsumpha3 <- lookupCountryPak conn ferilizerConsum (Year 2023)
    let arableHA3 = (combinePaks Multiply arablePC3 pops3){pDataSet = arableLandTotal}
        fertilizerConsumTot3 =
            (combinePaks Multiply arableHA3 fertConsumpha3)
                { pDataSet = fertilizerConsumptionTotal
                }

    -- fertConsumpc <- aggregate regionMembers2 conn ferilizerConsum2 (Year 2023) -- leer
    -- let fertilizerProd = combineRegionTables Divide fertilizerConsumTot fertConsumpc
    -- TODO must be handled with virtual dataset

    close conn

    let regionPaks =
            countryToRegionPaks
                regionMembers2
                [arableHA3, fertConsumpha3, fertilizerConsumTot3] ::
                [RegionPak3]
        mdCols = map wrapMdCol1 regionPaks
    -- , fertConsumpc
    -- , fertilizerProd
    -- MdColumn "arablHA (M ha)" Mega 0  arablHA
    -- , MdColumn "fertConsum (kg/ha)" Unit 0  fertConsumpha
    -- , MdColumn "fertilizerConsum Tot(G kg)" Giga 0  fertilizerConsumTot
    -- , MdColumn "Duengerverbrauch (% der Produktion)" Unit 0  fertConsumpc
    -- , MdColumn "Duengerproduktion (M kg)" Mega 0  fertilizerProd
    -- value is t
    -- value is 10**11 kg
    -- let sortedRegions = sortTerryByColumn Descending  pops3 --surfPerCap

    -- let md = markdownTable regionsList mdCols
    let md = markdownTable regionNames2 regionOrder mdCols
    putStrLn md
    writeTab2Table "tab23" md

writeTab2Table :: FilePath -> String -> IO ()
writeTab2Table filename contents = do
    createDirectoryIfMissing True tableOutputDirectory
    writeFile (tableOutputDirectory </> filename) contents
