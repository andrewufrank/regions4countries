------------------------------------------------------------------------------
--
-- Module      :   WorldBank.hs
-- read a csv file from the world bank and convert 
-----------------------------------------------------------------------------

module R4C.WorldBank ( importFile
, parseWorldBank
  ) where 

import UniformBase 
import R4C.Model 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Char (isDigit)
import Data.Scientific (Scientific)

importFile
    :: FilePath
    -> IO (Indicator,[Observation])

importFile file = do
    bytes <- BL.readFile file
    pure (parseWorldBank bytes)

parseWorldBank
    :: BL.ByteString
    -> (Indicator, [Observation])
parseWorldBank bytes =
    (indicator, observations)
  where
    rows = decodeCSV bytes

    header   = headerRow rows
    dataRows = countryRows rows

    firstRow =
        case dataRows of
            []    -> error "No data rows in World Bank file"
            r : _ -> r

    indicator =
        parseIndicator header firstRow

    years =
        yearColumns header

    countryCol =
        countryCodeColumn header

    observations =
        concatMap
            (parseCountry countryCol years indicator)
            dataRows


type Row    = V.Vector Text
type Header = Row

decodeCSV :: BL.ByteString -> [Row]
decodeCSV bytes =
    case Csv.decode Csv.NoHeader (dropPreamble bytes) of
        Left err ->
            error err

        Right rows ->
            map (V.map decode) (V.toList rows)
  where
    decode =
        TextEncoding.decodeUtf8 . BL.toStrict

dropPreamble :: BL.ByteString -> BL.ByteString
dropPreamble =
    BC.unlines
    . dropWhile (not . isHeader)
    . BC.lines
  where
    isHeader line =
        let txt = TextEncoding.decodeUtf8 (BL.toStrict line)
        in  "Country Name" `Text.isInfixOf` txt
         && "Country Code" `Text.isInfixOf` txt

-- dropUntilHeader :: [Row] -> [Row]
-- dropUntilHeader =
--     dropWhile (not . isHeader)

-- dropPreamble :: BL.ByteString -> BL.ByteString
-- dropPreamble =
--     BC.unlines
--     . dropWhile (not . isHeader)
--     . BC.lines
--   where
-- isHeader line =
--        "\"Country Name\"" `BC.isInfixOf` line
--     || "\"Country Code\"" `BC.isInfixOf` line
-- isHeader line =
--     "\"Country Name\",\"Country Code\"" `BC.isPrefixOf` line
-- isHeader line =
--         "\"Country Name\"" `BC.isPrefixOf` line

-- isHeader :: Row -> Bool
-- isHeader row =
--     not (V.null row)
--         && row V.! 0 == "Country Name" -- "Country Name"  

parseIndicator :: Header -> Row -> Indicator
parseIndicator hdr row =
    Indicator
        { indicatorId =
            IndicatorId (cell row (indicatorCodeColumn hdr))
        , indicatorName =
            cell row (indicatorNameColumn hdr)
        }

findColumnAny :: Header -> [Text] -> Int
findColumnAny hdr [] =
    error "None of the column names found."

findColumnAny hdr (n:ns) =
    case V.findIndex (== n) hdr of
        Just i  -> i
        Nothing -> findColumnAny hdr ns

indicatorCodeColumn :: Header -> Int
indicatorCodeColumn hdr =
    findColumnAny hdr
        [ "Series Code"
        , "Indicator Code"
        ]

indicatorNameColumn :: Header -> Int
indicatorNameColumn hdr =
    findColumnAny hdr
        [ "Series Name"
        , "Indicator Name"
        ]

countryCodeColumn :: Header -> Int
countryCodeColumn hdr =
    findColumn hdr "Country Code"

-- countryCol = countryCodeColumn header

findColumn :: Header -> Text -> Int
findColumn hdr name =
    case V.findIndex (== name) hdr of
        Just i  -> i
        Nothing -> error ("Column not found: " ++ Text.unpack name)

yearColumns
    :: Header
    -> [(Int,Year)]
yearColumns hdr =
    mapMaybe yearColumn (zip [0..] (V.toList hdr))

yearColumn :: (Int, Text) -> Maybe (Int, Year)
yearColumn (i, txt) =
    case parseYear txt of
        Just y  -> Just (i,y)
        Nothing -> Nothing

parseYear :: Text -> Maybe Year
parseYear txt =
    if length first4 == 4 && all isDigit first4
    then  Year <$> readMaybe first4
    else Nothing
  where
    first4 = take 4 (Text.unpack txt)

parseCountry
    :: Int  -- countryCol
    -> [(Int, Year)]
    -> Indicator
    -> Row
    -> [Observation]

parseCountry countryCol years indicator row =
    mapMaybe observation years
  where
    country =
        CountryId (cell row countryCol)

    observation (col, year) = do
        value <- parseValue (cell row col)

        pure Observation
            { obsCountry   = country
            , obsIndicator = indicatorId indicator
            , obsYear      = year
            , obsValue     = value
            }

parseValue
    :: Text
    -> Maybe Value

parseValue t
    | Text.null t  = Nothing
    | otherwise = Value <$> readMaybe (Text.unpack t)

-- saveIndicator db ind
-- saveObservations db obs

-- readFile :: FilePath -> IO ByteString
--         ↓
-- parseWorldBank :: ByteString -> Either Error (Indicator, [Observation])


-- HELPER

cell :: Row
    -> Int
    -> Text
-- get a cell from a row 
cell row i = row V.! i

headerRow :: [Row] -> Header
headerRow (hdr : _) = hdr
headerRow [] =
    error "empty World Bank file"

countryRows :: [Row] -> [Row]
countryRows (_hdr : rows) = rows
countryRows [] = []

