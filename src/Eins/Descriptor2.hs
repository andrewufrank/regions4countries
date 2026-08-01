-----------------------------------------------------------------------------
--
-- Module      :   new datasets
-- should become virtual datasets 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eins.Descriptor2
    where

import           Eins.Region (regionOrder)
import qualified Eins.Region as RBT
import qualified Data.Text as T
import           Database.SQLite.Simple
import           GHC.Generics (Generic1 (to1))
import           GHC.IO.Handle.Types (Handle__)
import           R4C.Aggregate
import           R4C.Export.CountnryCodeNames
import           R4C.Export.Markdown (writeMarkdownBlock, writeMarkdownIncludes)
import           R4C.Export.Table
import           R4C.Import.Database
import           R4C.Import.Query
import           R4C.Model
import           R4C.Pak
import qualified Eins.Region3 as R3
import           Eins.Region3
import           R4C.Statistics
import           R4C.Territory
import           Eins.Config
import           Eins.Descriptor
import           Eins.Region2
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           UniformBase hiding ((</>))

-- should become a virtual dataset
agriLand :: Dataset
agriLand =
    Dataset
        { dsShortName = "Landwirtschaft"
        , dsName = "Surface area (sq. km)"
        , dsDefinition =
            " ."
        , dsUnit = "km\178"
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Kilo
        , dsExtensive = True
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
        --     "FAO  "
        }
useableLand :: Dataset
useableLand =
    Dataset
        {  dsShortName = "Nutzbares Land"
        , dsName = "useable Land area (sq. km)"
        -- , dsDefinition =
        --     " ."
        , dsUnit = "km\178"
        -- , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = True
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }
useableLandPerCent :: Dataset
useableLandPerCent =
    Dataset
        {  dsShortName = "Nutzbares Land"
        , dsName = "useableLand PerCent"
        -- , dsDefinition =
        --     " ."
        , dsUnit = "%"
        -- , dsAggregation = Sum
        , dsDecimals = 2
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }

useableLandPC:: Dataset
useableLandPC =
    Dataset
        {  dsShortName = "Nutzbare Landflaeche"
        , dsName = "useableLand per Capita"
        -- , dsDefinition =
        --     " ."
        , dsUnit = "a/P"
        -- , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Unit
        , dsExtensive = False
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }

fertilityCount =
   Dataset
        {  dsShortName = "Kinder geboren"
        , dsName = "children born per year"
        -- , dsDefinition =
        --     " ."
        , dsUnit = "P/y"
        -- , dsAggregation = Sum
        , dsDecimals = 0
        , dsScale = Mega
        , dsExtensive = True
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }

netMigrationCount =    Dataset
        {  dsShortName = "MigrationRate netto "
        , dsName = "net migration per year per M person "
        -- , dsDefinition =
        --     " ."
        , dsUnit = "P/Py"
        -- , dsAggregation = Sum
        , dsDecimals = 3
        , dsScale = Kilo
        , dsExtensive = False
        , dsLastYear = Nothing
        -- , dsSourceOrganization =
            -- "FAO  "
        }

popGrowthCount = Dataset
        {dsIndicator = IndicatorId {unIndicatorId = "GrowthCount"}
        , dsShortName = "Wachstum Bevoelkerung"
        , dsName = "population growth annual "
        , dsDefinition =
            "Txxx"
        , dsUnit = "P/y"
        , dsAggregation = Sum -- WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 0
        , dsScale = Mega
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

gnpcc = Dataset
        {dsIndicator = IndicatorId {unIndicatorId = "GNPpercapita2country"}
        , dsShortName = "GNP2c"
        , dsName = "GNP2c"
        , dsDefinition =
            "Txxx"
        , dsUnit = "$x"
        , dsAggregation = Sum -- WeightedBy (IndicatorId "SP.POP.TOTL")
        , dsDecimals = 0
        , dsScale = Mega
        , dsExtensive = True
        , dsLastYear = Nothing
        , dsSourceOrganization =
            "xxx"
        }

 