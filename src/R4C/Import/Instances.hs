-----------------------------------------------------------------------------
--
-- Module      :   instances for database sqlite

-----------------------------------------------------------------------------

module R4C.Import.Instances
where

import UniformBase

import qualified Data.Scientific as Sc
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow

-- import Database.SQLite.Simple
import qualified Data.Text as Text
import R4C.Model
import Text.Read (readMaybe)

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

instance ToField DataSource where
    toField = toField . Text.pack . show

instance FromField DataSource where
    fromField f = do
        txt <- fromField f
        case readMaybe (Text.unpack txt) of
            Just dataSource -> pure dataSource
            Nothing ->
                returnError
                    ConversionFailed
                    f
                    ("invalid data source: " ++ Text.unpack txt)

instance ToRow IndicatorRef where
    toRow ref = [toField (refSource ref), toField (refIndicator ref)]

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
instance FromRow CountryValue where
    fromRow = TerryValue <$> field <*> field

instance FromRow Observation where
    fromRow =
        ( \country source indicator -> Observation country (IndicatorRef source indicator)
        )
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field

instance ToRow Observation where
    toRow o =
        [ toField (obsCountry o)
        , toField (refSource (obsIndicator o))
        , toField (refIndicator (obsIndicator o))
        , toField (obsYear o)
        , toField (obsValue o)
        ]

instance FromRow YearValue where
    fromRow =
        YearValue
            <$> field
            <*> field

instance ToRow Country where
    toRow c =
        [ toField (countryId c)
        , toField (countryName c)
        , toField (countryRegion c)
        , toField (countryIncomeGroup c)
        , toField (countrySpecialNotes c)
        ]

instance FromRow Country where
    fromRow =
        Country
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field

-- instance ToField Aggregation where
--     toField =
--         SQLText . pack . show

-- instance FromField Aggregation where
--     fromField f = do
--         txt <- fromField f
--         case readMaybe (unpack txt) of
--             Just a  -> pure a
--             Nothing ->
--                 returnError
--                     ConversionFailed
--                     f
--                     "invalid aggregation"
aggregationToText ::
    Aggregation ->
    Text
aggregationToText Sum =
    "Sum"
aggregationToText Mean =
    "Mean"
aggregationToText (WeightedBy ind) =
    "WeightedBy:"
        <> Text.pack (show (refSource ind))
        <> ":"
        <> unIndicatorId (refIndicator ind)

textToAggregation ::
    Text ->
    Aggregation
textToAggregation "Sum" =
    Sum
textToAggregation "Mean" =
    Mean
textToAggregation txt
    | "WeightedBy:" `Text.isPrefixOf` txt =
        case Text.splitOn ":" (Text.drop 11 txt) of
            [sourceText, code] ->
                case readMaybe (Text.unpack sourceText) of
                    Just dataSource -> WeightedBy (IndicatorRef dataSource (IndicatorId code))
                    Nothing -> error ("Unknown data source: " ++ Text.unpack sourceText)
            _ -> error ("Invalid weighted aggregation: " ++ Text.unpack txt)
    | otherwise =
        error ("Unknown aggregation: " ++ Text.unpack txt)

instance ToField Aggregation where
    toField =
        toField . aggregationToText

instance FromField Aggregation where
    fromField f = do
        txt <- fromField f
        pure (textToAggregation txt)

instance FromRow Indicator where
    fromRow =
        Indicator
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

-- <*> (read <$> field)
