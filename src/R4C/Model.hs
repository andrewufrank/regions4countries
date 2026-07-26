-----------------------------------------------------------------------------
--
-- Module      :   Model.hs

-- A useful rule of thumb:

-- Model.hs: "How do I convert values?"
-- Database.hs: "How do I store and retrieve things?"
-- WorldBank.hs: "How do I interpret external files?"
-- Orchestrator.hs: "How do I connect the pieces?"
-----------------------------------------------------------------------------

module R4C.Model  
     where
import UniformBase  

import qualified Data.Scientific as Sc
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
-- import Database.SQLite.Simple 
import qualified Data.Text as Text
import Text.Read (readMaybe)

-- | world bank indicator code, eg SP.POP.TOTL
newtype IndicatorId =
    IndicatorId
        { unIndicatorId :: Text
        }
    deriving (Eq, Ord, Show, Read)

-- data Unit
--     = Persons
--     | Percent
--     | USD
--     | Years
--     | SquareKm
--     | Tonnes

data Aggregation
    = Sum
    | Mean
    | WeightedBy IndicatorId
    deriving (Eq, Ord, Show, Read)

data Indicator = Indicator
-- What is observed 
    { indicatorId   :: IndicatorId
    , indicatorName :: Text
    , sourceNote         :: Text
    , sourceOrganization :: Text    
    -- , aggregation   :: Aggregation  -- is not from WB and perhaps not belongs here?
    }
    deriving (Eq, Ord, Show)

-- | Dataset is my description (indicaor is the WorldBank description)
data Dataset = Dataset
    { dsIndicator          :: IndicatorId
    , dsShortName          :: Text
    , dsName               :: Text
    , dsDefinition         :: Text
    , dsUnit               :: Text
    , dsAggregation        :: Aggregation
    , dsDecimals           :: Int
    , dsScale                :: Scale 
    , dsExtensive          :: Bool
    , dsLastYear           :: Maybe Year
    , dsSourceOrganization :: Text
    }
    deriving (Eq, Ord, Show)

newtype Year = Year Int
    deriving (Eq, Ord, Show)

data YearValue = YearValue
    { yvYear  :: Year
    , yvValue :: Value
    }
    deriving (Eq, Ord, Show)

newtype Value = Value Sc.Scientific
    deriving (Eq, Ord, Show)
    -- to keep scientifi local

-- data Value = Count Int | Measure Double | Percentage Double | Money Double 

data Observation = Observation
    { obsCountry   :: CountryId
    , obsIndicator :: IndicatorId
    , obsYear      :: Year
    , obsValue     :: Value
    }
    deriving (Eq, Ord, Show)

-------------------------------------------------------OLD -the table to generalize
-- type CountryTable = [CountryValue]

-- type CountryPairs = [(CountryValue, CountryValue)]

-- data CountryValue = CountryValue
--     { cvCountry :: CountryId
--     , cvValue   :: Maybe Double
--     }
--     deriving (Eq, Ord, Show)

-- type RegionTableX a = [RegionValueX a]
type RegionTableX a = [RegionValue]
-- type RegionTable = RegionTableX Double 
type RegionValueX = TerryValue RegionId Double 
-- data RegionValueX a = RegionValue
--     { rvRegion :: RegionId
--     , rvValue  :: Maybe a
--     }
--     deriving (Eq, Ord, Show)
-- type RegionValue = RegionValueX Double 

-----------------------------------Territories 
class ShowTerryId a where 
    showTerryId :: a -> Text 

instance ShowTerryId RegionId where 
    showTerryId :: RegionId -> Text
    showTerryId (RegionId c) = c
instance ShowTerryId CountryId where 
    showTerryId (CountryId c) = c 

data TerryName t = Terry
    { terryId   :: t
    , terryName :: Text
    }
    deriving (Eq, Ord, Show)

data TerryValue t v = TerryValue 
    { tvCode :: t
    , tvValue :: Maybe v }
    deriving (Eq, Ord, Show)

--------------------------------------------Regions and Countries

-- | ISO 3166-1 alpha-3 country code
newtype CountryId = CountryId Text
    deriving (Eq, Ord, Show)
unCountryId (CountryId code) = code 

data Country = Country
    { countryId   :: CountryId
    , countryName :: Text  -- ^ english, the WB tabble name
    , countryRegion    :: Text -- ^ the WB region 
    , countryIncomeGroup :: Text -- ^ the WB incomeGroup
    , countrySpecialNotes :: Text -- ^ the WB specialNotes 
    }
    deriving (Eq, Ord, Show)

type CountryName = TerryName CountryId 

newtype RegionId = RegionId Text 
    deriving (Eq, Ord, Show)

data Region = Region
    { regionId   :: RegionId
    , regionName :: Text
    }
    deriving (Eq, Ord, Show)

type RegionMembers = [(RegionId, [CountryId])]

countriesInRegion :: RegionMembers -> RegionId -> [CountryId]
-- get all countries in a region (could be a map but list is short)
countriesInRegion memberships region =
    case lookup region memberships of
        Just cs -> cs
        Nothing -> []

type RegionName = TerryName RegionId

type CountryValue = TerryValue CountryId Double 
type CountryTable = TerryTable CountryId ( Double)

type RegionValue = TerryValue RegionId Double  -- RegionValueX Double 
type RegionTable  = MdColumn RegionId Double  -- TerryTable RegionId ( Double) -- [RegionValue]
            -- depreciate 

type RegionTable2 = [(RegionId, CountryTable)] -- new format
type RegionTable3 = (Dataset, [(RegionId, CountryTable)]) -- new format

type TerryTable t v = [TerryValue t v]

type CountryPairs = [(CountryValue, CountryValue)]
type RegionPairs = [(RegionValue, RegionValue)]

type TerryPairs t v  = [(TerryValue t v, TerryValue t v )]

data Scale = Kilo | Mega | Giga | Tera | Centi | Unit | Milli| Micro | Nano | Pico deriving (Eq, Ord, Show )

show1scale s = case s of 
    Kilo -> "k"
    Mega -> "M"
    Unit -> ""
    _ -> ""

data MdColumn t v =   MdColumn
    { colTitle    :: String
    , colScale    :: Scale
    , colUnit    :: Text 
    , colDecimals :: Int
    , colValues   :: TerryTable t v
    } 
    deriving (Eq, Ord, Show)