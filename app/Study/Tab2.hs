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

xcountries = map CountryId ["FIN", "AUT", "BRA", "USA", "RUS"]

getData21 :: IO ()
-- fig21 cereal production and veg. consumption
-- fig23 
getData21 = do
    conn <- open dbPath
    pop3 <- lookupCountryPak conn (population) (Year 2024)
    cerealProd3 <- lookupCountryPak conn cerealProduction (Year 2023) -- 2024 not all values
    -- AG.PRD.CREL.MT
    -- for tab 23
    arablePC3 <- setPakUnitScale "a/P" . scalePak 100 <$> lookupCountryPak conn arableLandPC (Year 2023)
    fertConsumpha3 <- lookupCountryPak conn ferilizerConsum (Year 2023)
    close conn

    let c3 = [pop3, cerealProd3, arablePC3, fertConsumpha3 ]
-- test data availability
        mdCountry = markdownPakTable allCodeNames xcountries c3 -- less1m mdC
        testdata = markdownPakTable allCodeNames xcountries c3 -- less1m mdC
    putIOwords ["test data availability\n", s2t testdata]

    let cerealFood3 = (scalePak 0.1 pop3){pDataSet = cerealFood} -- 100 kg per head
        cerealDomUse3 = (scalePak 2.5 cerealFood3){pDataSet = cerealDomesticUse}
        potExport3 =
            (combinePaks Subtract cerealProd3 cerealDomUse3)
                -- { pDataSet = potentialCerealExport
                -- }
    -- for ab 23 
    let arableHA3 =  setPakShortName "Verbrauch gesamt" $ (combinePaks Multiply arablePC3 pop3)
                -- {pDataSet = arableLandTotal}
        fertilizerConsumTot3 = setPakShortName "pot. Export" $
            (combinePaks Multiply arableHA3 fertConsumpha3)
                -- { pDataSet = fertilizerConsumptionTotal
                -- }

    let c21 = [cerealFood3, cerealDomUse3, potExport3 ]
        c23 = [arableHA3, fertilizerConsumTot3]


    -- construct tables 
        tab21paks = [cerealProd3, cerealFood3, cerealDomUse3, potExport3]
        tab23paks = c3 ++ c23 
    --- test country tables 
    let c3' =  [c3, c21, c23, tab21paks, tab23paks] :: [[CountryPak3]]
    let tabNames = ["c3", "c21", "c23", "tab21", "tab23"]  
        ctables = map  makeCountryTables $ zip tabNames c3'
    putIOwords ["the country tables\n", unlines'  ctables]

    -- the region tables 
    let regionTables = map makeRegionTables $ zip tabNames c3' 
    putIOwords ["the region tables\n", unlines'  regionTables]

    -- let regionPaks =
    --         countryToRegionPaks regionMembers2 [cerealProd3, cerealFood3] ::
    --             [RegionPak3]
    --     mdCols = map wrapMdCol1 regionPaks

    -- let md = markdownTable regionNames2 regionOrder mdCols
    -- putStrLn md
    -- writeTab2Table "tab21" md

-- writeTab1Table "tab11" mdRegion
makeCountryTables :: (Text ,  [CountryPak3]) -> Text
makeCountryTables (name, regs) = "\n" <> name <> "\n" 
            <> s2t  (markdownPakTable allCodeNames xcountries regs)


-- makeRegionTabs :: [Pak CountryId (WObs Double)] -> String
makeRegionTables :: (Text ,  [CountryPak3]) -> Text
makeRegionTables (name, regs) = "\n" <> name <> "\n" <> s2t  (markdownPakTable regionNames2 regionOrder2 
            $ countryToRegionPaks regionMembers2 regs)


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
