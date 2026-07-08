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

-- |  ISO 3166-1 alpha-3 country code
newtype CountryId = CountryId Text
    deriving (Eq, Ord, Show)

data Country = Country
    { countryId   :: CountryId
    , countryName :: Text
    -- , countryRegion    :: RegionId  -- country - region is many to many 
    }
    deriving (Eq, Ord, Show)

newtype RegionId = RegionId Text 
    deriving (Eq, Ord, Show)

data Region = Region
    { regionId   :: RegionId
    , regionName :: Text
    }

data CountryRegion = CountryRegion
    { crCountry :: CountryId
    , crRegion  :: RegionId
    }
    deriving (Eq, Ord, Show)


newtype IndicatorId = IndicatorId Text
-- | world bank indicator code, eg SP.POP.TOTL
    deriving (Eq, Ord, Show)

-- data Unit
--     = Persons
--     | Percent
--     | USD
--     | Years
--     | SquareKm
--     | Tonnes
    
data Indicator = Indicator
-- What is observed 
    { indicatorId   :: IndicatorId
    , indicatorName :: Text
--     , indicatorUnit :: Unit  -- fill that separately
    }
    deriving (Eq, Ord, Show)

newtype Year = Year Int
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
----------
instance ToField CountryId where
    toField (CountryId t) = toField t

instance FromField CountryId where
    fromField f = CountryId <$> fromField f

instance ToField RegionId where
    toField (RegionId t) = toField t

instance FromField RegionId where
    fromField f = RegionId <$> fromField f

instance ToField IndicatorId where
    toField (IndicatorId t) = toField t

instance FromField IndicatorId where
    fromField f = IndicatorId <$> fromField f

instance ToField Year where
    toField (Year y) = toField y

instance FromField Year where
    fromField f = Year <$> fromField f

-- instance ToField Value where
--     toField (Value v) = toField v

-- instance FromField Value where
--     fromField f = Value <$> fromField f

instance ToField Value where
    toField (Value v) =
        toField (Sc.toRealFloat v :: Double)

-- instance FromField Value where
--     fromField f =
--         Value . Sc.fromFloatDigits <$> (fromField f)
instance FromField Value where
    fromField f = do
        d <- fromField f
        pure (Value (Sc.fromFloatDigits (d :: Double)))

--------
instance FromRow Observation where
    fromRow =
        Observation
            <$> field
            <*> field
            <*> field
            <*> field


instance ToRow Observation where
    toRow o =
      [ toField (obsCountry o)
      , toField (obsIndicator o)
      , toField (obsYear o)
      , toField (obsValue o)
      ]

data YearValue = YearValue
    { yvYear  :: Year
    , yvValue :: Value
    }
    deriving Show
instance FromRow YearValue where
    fromRow =
        YearValue
            <$> field
            <*> field
                