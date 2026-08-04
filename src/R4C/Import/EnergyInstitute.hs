module R4C.Import.EnergyInstitute (
    energyInstituteIndicators,
    readEnergyInstituteObservations,
) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import R4C.Model

data EnergyRow = EnergyRow
    { eiCountry :: T.Text
    , eiYear :: Int
    , eiCode :: T.Text
    , eiVariable :: T.Text
    , eiValue :: Double
    }

instance FromNamedRecord EnergyRow where
    parseNamedRecord row =
        EnergyRow
            <$> row .: "Country"
            <*> row .: "Year"
            <*> row .: "ISO3166_alpha3"
            <*> row .: "Var"
            <*> row .: "Value"

-- perhaps add 
-- |tes_ej | Total energy supply | EJ |
-- | tes_gj_pc | Total energy supply per capita | GJ/person |
-- | oil_tes_ej | Oil | EJ |
-- | gas_tes_ej | Natural gas | EJ |
-- | coal_tes_ej | Coal | EJ |

energyInstituteIndicators :: [Indicator]
energyInstituteIndicators =
    [ indicator "oilcons_mt" "Oil consumption"
    , indicator "oilprod_mt" "Oil production"
    , indicator "gascons_bcm" "Natural gas consumption"
    , indicator "gasprod_bcm" "Natural gas production"
    ]
  where
    indicator code name =
        Indicator
            { source = EnergyInstitute
            , indicatorId = IndicatorId code
            , indicatorName = name
            , sourceNote = "Statistical Review of World Energy, narrow-format CSV"
            , sourceOrganization = "Energy Institute"
            , aggregation = Sum
            }

readEnergyInstituteObservations ::
    FilePath -> Year -> IO [Observation]
readEnergyInstituteObservations path requestedYear = do
    bytes <- BL.readFile path
    case decodeByName bytes of
        Left message -> fail ("Energy Institute CSV: " ++ message)
        Right (_, rows) -> pure (foldr addObservation [] (V.toList rows))
  where
    selected = map (indicatorId) energyInstituteIndicators

    addObservation row result
        | Year (eiYear row) /= requestedYear = result
        | IndicatorId (eiVariable row) `notElem` selected = result
        | not (ordinaryCountryCode (eiCode row)) = result
        | otherwise =
            Observation
                { obsCountry = CountryId (eiCode row)
                , obsIndicator =
                    IndicatorRef EnergyInstitute (IndicatorId (eiVariable row))
                , obsYear = requestedYear
                , obsValue = realToValue (eiValue row)
                }
                : result

    realToValue = Value . realToFrac

ordinaryCountryCode :: T.Text -> Bool
ordinaryCountryCode code =
    T.length code == 3
        && T.all (`elem` ['A' .. 'Z']) code
        && code `notElem` ["ROW", "WLD", "SUN"]
        -- collect only countries with 3 char country codes
