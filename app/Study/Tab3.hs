-----------------------------------------------------------------------------
--
-- Module      :   Tab3 bodenschaetze besonders oil  & gas
-- for each region:
--

-----------------------------------------------------------------------------

module Tab3
where

import Database.SQLite.Simple
import Eins.Config
import Eins.Descriptor
import Eins.Region (regionOrder)
import Eins.Region2
import Eins.BlocDefs
import Eins.Region3 hiding (regionOrder)
import R4C.Export.CountnryCodeNames
import R4C.Export.Table
import R4C.Import.Database
import R4C.Import.Query
import R4C.Model
import R4C.Pak
import R4C.Territory
import R4C.TerryTable
import StudyTable
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), combine)
import UniformBase hiding (uncurry, (</>))
import Data.IntMap (isSubmapOfBy)

xcountries = map CountryId ["FIN", "AUT", "BRA", "USA", "RUS"]


getData31 :: IO ()
-- fig21 cereal production and veg. consumption
-- fig23
getData31 = do
    conn <- open dbPath
    pop3 <- lookupCountryPak conn (population) (Year 2025)
    oilprod3 <- lookupCountryPak conn oilProduction (Year 2025)
    oilcons3 <- lookupCountryPak conn oilConsumption (Year 2025)
    gasprod3 <- lookupCountryPak conn gasProduction (Year 2025)
    gascons3 <- lookupCountryPak conn gasConsumption (Year 2025)

    close conn

    let
        oilexp3 = setPakShortName "Diff. Öl" $ combinePaks Subtract oilprod3 oilcons3
        gasexp3 = setPakShortName "Diff. Gas" $ combinePaks Subtract gasprod3 gascons3 

    let c3 = [oilprod3, oilcons3, oilexp3, gasprod3, gascons3, gasexp3]
        -- test data availability
    let mdCountry = markdownPakTable allCodeNames xcountries c3 -- less1m mdC
        testdata = markdownPakTable allCodeNames xcountries c3 -- less1m mdC
    putIOwords ["test data availability\n", s2t testdata]



        -- convert to EJ
    let oilprod4 = setPakUnitScale "EJ" $ scalePak bmt2EJ oilprod3
        oilcons4 = setPakUnitScale "EJ" $ scalePak bmt2EJ oilcons3
        gasprod4 = setPakUnitScale "EJ" $ scalePak bcm2EJ gasprod3
        gascons4 = setPakUnitScale "EJ" $ scalePak bcm2EJ gascons3

        oilexp4 = setPakShortName "Diff. Öl" $ combinePaks Subtract oilprod4 oilcons4
        gasexp4 = setPakShortName "Diff. Gas" $ combinePaks Subtract gasprod4 gascons4 

        prod4 = setPakShortName "Energie Produktion" $ combinePaks Add oilprod4 gasprod4 
        cons4 = setPakShortName "Energeie Verbrauch" $ combinePaks Add oilcons4 gascons4 
        exp4 = setPakShortName "Diff. Energie" $ combinePaks Subtract prod4 cons4 

    let c31 = [oilprod4, oilcons4, oilexp4, gasprod4, gascons4, gasexp4]
        c32 = [prod4, cons4, exp4]
    --         c23 = [arableHA3, fertilizerConsumTot3]

    --     -- construct tables
    --         tab21paks = [cerealProd3, cerealFood3, cerealDomUse3, potExport3]
    --         tab23paks = c3 ++ c23
    --- test country tables
    let c3' = [c3, c31, c32] -- c21, c23, tab21paks, tab23paks] :: [[CountryPak3]]
    let tabNames = ["c3", "c31", "c32", "tab21", "tab23"]
        ctables = map (makeCountryTables xcountries) $ zip tabNames c3'
    putIOwords ["the country tables\n", unlines' ctables]

    -- the region tables
    let regionTables = map makeRegionTables $ zip tabNames c3'
    putIOwords ["the region tables\n", unlines' regionTables]

    let c5 = (countryToRegionPaks regionMembers2) c3 -- [("c3", c3)]
        c5tab = makeBlocTables ("c3", c5)
    putIOwords ["the bloc tables\n", unlines' [c5tab]]

    return ()


-- makeBlocTables :: (Text, [RegionPak3]) -> Text
-- | print a number of regiion paks with each a tile

makeBlocTables (name, paks) =
    "\n"
        <> name
        <> "\n\n"  -- braucht leerzeile fuer markdown 
        <> s2t
            ( markdownPakTable
                blocNames2
                blocOrder2
                (countryToRegionPaks blocMembers2  $ paks)
            )
--  . countryToRegionPaks regionMembers2