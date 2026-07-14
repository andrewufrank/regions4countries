------------------------------------------------------------------------------
--
-- Module      :   WorldBank.hs
-- read a csv file from the world bank and convert 
-----------------------------------------------------------------------------

module R4C.WorldBank  where 

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

readIndicatorFile
    :: FilePath
    -> IO ([Observation])

readIndicatorFile file = do
    bytes <- BL.readFile file
    pure (parseWBindicator bytes)

readIndicatorMetadataFile
    :: FilePath
    -> IO [Indicator]
readIndicatorMetadataFile file = do
    bytes <- BL.readFile file
    pure (parseWBindicatorMetadata bytes)

readCountryMetadataFile
    :: FilePath
    -> IO [Country]
readCountryMetadataFile file = do
    bytes <- BL.readFile file
    pure (parseWBcountries bytes)


parseWBindicator
    :: BL.ByteString
    -> ([Observation])
-- | parse a WorldBank Indicator csv file 
parseWBindicator bytes =
    case decodeCSV . dropPreamble $ bytes of
        [] ->
            error "empty World Bank file"
        header : dataRows ->
            case dataRows of
                [] ->
                    error "No data rows in World Bank file"
                firstRow : _ ->
                    (observations)
                  where
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


parseWBcountries bytes =
    map parseCountryRow dataRows
  where
    rows =
        decodeCSV bytes

    header : dataRows =
        rows

    countryCodeCol =
        countryCodeColumn header

    tableNameCol =
        tableNameColumn header

    regionCol =
        regionColumn header

    incomeGroupCol =
        incomeGroupColumn header

    specialNotesCol =
        specialNotesColumn header

    parseCountryRow row =
        Country
            { countryId =
                CountryId (cell row countryCodeCol)
            , countryName =
                cell row tableNameCol
            , countryRegion =
                cell row regionCol
            , countryIncomeGroup =
                cell row incomeGroupCol
            , countrySpecialNotes =
                cell row specialNotesCol
            }

parseWBindicatorMetadata
    :: BL.ByteString
    -> [Indicator]

parseWBindicatorMetadata bytes =
    map parse rows
  where
    header : rows =
        decodeCSV (stripBom bytes)

    codeCol =
        findColumn header "INDICATOR_CODE"

    nameCol =
        findColumn header "INDICATOR_NAME"

    noteCol =
        findColumn header "SOURCE_NOTE"

    orgCol =
        findColumn header "SOURCE_ORGANIZATION"

    parse row =
        Indicator
            { indicatorId =
                IndicatorId (cell row codeCol)

            , indicatorName =
                cell row nameCol

            , sourceNote =
                cell row noteCol

            , sourceOrganization =
                cell row orgCol
            }

type Row    = V.Vector Text
type Header = Row

decodeCSV :: BL.ByteString -> [Row]
decodeCSV bytes =
    case Csv.decode Csv.NoHeader (stripBom bytes) of
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

tableNameColumn :: Header -> Int
tableNameColumn hdr =
    findColumn hdr "TableName"

regionColumn :: Header -> Int
regionColumn hdr =
    findColumn hdr "Region"

incomeGroupColumn :: Header -> Int
incomeGroupColumn hdr =
    findColumn hdr "IncomeGroup"

specialNotesColumn :: Header -> Int
specialNotesColumn hdr =
    findColumn hdr "SpecialNotes"

findColumn :: Header -> Text -> Int
findColumn hdr name =
    case V.findIndex (== name) hdr of
        Just i  -> i
        Nothing -> error ("Column not found: " ++ Text.unpack name)

---- end of find column header

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

-- HELPER

cell :: Row
    -> Int
    -> Text
-- get a cell from a row 
cell row i = row V.! i

stripBom :: BL.ByteString -> BL.ByteString
stripBom bs
    | BL.isPrefixOf bom bs = BL.drop 3 bs
    | otherwise            = bs
  where
    bom = BL.pack [0xEF,0xBB,0xBF]

-- headerRow :: [Row] -> Header
-- headerRow (hdr : _) = hdr
-- headerRow [] =
--     error "empty World Bank file"

-- countryRows :: [Row] -> [Row]
-- countryRows (_hdr : rows) = rows
-- countryRows [] = []

