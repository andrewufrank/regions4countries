module StudyTable (
    makeCountryTables,
    makeRegionTables,
) where

import Data.Text (Text)
import Eins.Region2
import R4C.Export.CountnryCodeNames
import R4C.Export.Table
import R4C.Model
import R4C.Pak
import UniformBase

makeCountryTables :: [CountryId] -> (Text, [CountryPak3]) -> Text

-- | print a number of country paks with each a tile
makeCountryTables countries (name, paks) =
    "\n"
        <> name
        <> "\n"
        <> s2t (markdownPakTable allCodeNames countries paks)

makeRegionTables :: (Text, [CountryPak3]) -> Text

-- | print a number of regiion paks with each a tile
makeRegionTables (name, paks) =
    "\n"
        <> name
        <> "\n\n"  -- braucht leerzeile fuer markdown 
        <> s2t
            ( markdownPakTable
                regionNames2
                regionOrder2
                (countryToRegionPaks regionMembers2 paks)
            )
