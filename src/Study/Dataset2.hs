-- | The version-controlled registry of the indicators used by this study.
module Study.Dataset2
      where

import Data.List (find)
import qualified Data.Text as T

import R4C.Model

placeholder :: T.Text
placeholder = "TODO"

-- | A template for an indicator discovered in the database.  Its aggregation
-- and presentation fields remain intentionally local placeholders.
dummyDatasetFromIndicator :: Indicator -> Dataset
dummyDatasetFromIndicator ind =
    Dataset
        { dsIndicator = indicatorId ind
        , dsShortName = placeholder
        , dsName = indicatorName ind
        , dsDefinition = sourceNote ind
        , dsUnit = placeholder
        , dsAggregation = Sum
        , dsDecimals = 0
        , dsExtensive = False
        , dsLastYear = Nothing
        , dsSourceOrganization = sourceOrganization ind
        }


-- -- Keep one entry per ID.  energyConsum used the same code as migrationNet;
-- -- migrationNet is retained as the configured definition.
-- namedDatasets :: [(String, Dataset)]
-- namedDatasets =
--     [ ("energyConsum", energyConsum), ("waterConsum", waterConsum)
--     , ("agrarland", agrarland), ("cerealProduction", cerealProduction)
--     , ("agriculturalLand", agriculturalLand)
--     , ("agriculturalLandPC", agriculturalLandPC)
--     , ("ferilizerConsum2", ferilizerConsum2)
--     , ("ferilizerConsum", ferilizerConsum), ("migrationNet", migrationNet)
--     , ("fertilityRate", fertilityRate), ("getreideErtrag", getreideErtrag)
--     , ("population", population), ("surfaceArea", surfaceArea)
--     , ("grossNatProd", grossNatProd), ("gnpPPpc", gnpPPpc)
--     ]

-- datasets :: [Dataset]
-- datasets =
--     [ waterConsum, agrarland, cerealProduction, agriculturalLand
--     , agriculturalLandPC, ferilizerConsum2, ferilizerConsum, migrationNet
--     , fertilityRate, getreideErtrag, population, surfaceArea, grossNatProd
--     , gnpPPpc
--     ]

-- lookupDataset :: IndicatorId -> Maybe Dataset
-- lookupDataset iid = find ((== iid) . dsIndicator) datasets

data DatasetWarning
    = MetadataChanged IndicatorId T.Text T.Text T.Text
    | DatasetMissingFromDatabase IndicatorId
    deriving (Eq, Show)

-- | Merge World Bank metadata by ID.  A TODO value is filled silently on the
-- first run; later changes are reported.  The third result is a list of
-- placeholder records for database indicators not configured in this module.
reconcileDatasets :: [Dataset] -> [Indicator] -> ([DatasetWarning], [Dataset], [Dataset])
reconcileDatasets configured imported =
    ( concatMap warningsFor configured
    , map refresh configured
    , [ dummyDatasetFromIndicator ind
      | ind <- imported
      , not (any ((== indicatorId ind) . dsIndicator) configured)
      ]
    )
  where
    lookupImported ds = find ((== dsIndicator ds) . indicatorId) imported

    warningsFor ds =
        case lookupImported ds of
            Nothing -> [DatasetMissingFromDatabase (dsIndicator ds)]
            Just ind -> concat
                [ changed "name" (dsName ds) (indicatorName ind)
                , changed "definition" (dsDefinition ds) (sourceNote ind)
                , changed "source organization" (dsSourceOrganization ds) (sourceOrganization ind)
                ]
      where
        changed field old new
            | old == placeholder || old == new = []
            | otherwise = [MetadataChanged (dsIndicator ds) field old new]

    refresh ds =
        case lookupImported ds of
            Nothing -> ds
            Just ind -> ds
                { dsName = indicatorName ind
                , dsDefinition = sourceNote ind
                , dsSourceOrganization = sourceOrganization ind
                }

renderWarning :: DatasetWarning -> String
renderWarning (DatasetMissingFromDatabase iid) =
    "WARNING: configured indicator is absent from database: " ++ show iid
renderWarning (MetadataChanged iid field old new) =
    unlines
        [ "WARNING: World Bank metadata changed for " ++ show iid ++ " (" ++ T.unpack field ++ ")"
        , "  Dataset2: " ++ T.unpack old
        , "  database: " ++ T.unpack new
        ]

-- | Paste the result into 'datasets' after reviewing all TODO values.
renderDatasetTemplate :: Dataset -> String
renderDatasetTemplate ds =
    unlines
        [ "    Dataset"
        , "        { dsIndicator = " ++ renderIndicatorId (dsIndicator ds)
        , "        , dsShortName = \"TODO\""
        , "        , dsName = " ++ show (dsName ds)
        , "        , dsDefinition = " ++ show (dsDefinition ds)
        , "        , dsUnit = \"TODO\""
        , "        , dsAggregation = Sum  -- TODO: verify"
        , "        , dsDecimals = 0  -- TODO: verify"
        , "        , dsExtensive = False  -- TODO: verify"
        , "        , dsLastYear = Nothing"
        , "        , dsSourceOrganization = " ++ show (dsSourceOrganization ds)
        , "        }"
        ]

-- | Render a complete, editable declaration.  Definitions are wrapped at a
-- conservative width so the resulting source remains below 80 columns.
renderNamedDataset :: (String, Dataset) -> String
renderNamedDataset (name, ds) =
    unlines $
        [ name ++ " :: Dataset"
        , name ++ " ="
        , "    Dataset"
        , "        { dsIndicator = " ++ show (dsIndicator ds)
        , "        , dsShortName = " ++ showText (dsShortName ds)
        , "        , dsName = " ++ showText (dsName ds)
        ]
        ++ renderWrappedTextField "dsDefinition" (dsDefinition ds)
        ++ [ "        , dsUnit = " ++ showText (dsUnit ds)
           , "        , dsAggregation = " ++ renderAggregation (dsAggregation ds)
           , "        , dsDecimals = " ++ show (dsDecimals ds)
           , "        , dsExtensive = " ++ show (dsExtensive ds)
           , "        , dsLastYear = " ++ show (dsLastYear ds)
           ]
        ++ renderWrappedTextField "dsSourceOrganization" (dsSourceOrganization ds)
        ++ [ "        }"
           ]
  where
    showText = show . T.unpack

renderIndicatorId :: IndicatorId -> String
renderIndicatorId (IndicatorId iid) = "IndicatorId " ++ show (T.unpack iid)

renderAggregation :: Aggregation -> String
renderAggregation Sum = "Sum"
renderAggregation Mean = "Mean"
renderAggregation (WeightedBy iid) = "WeightedBy (" ++ renderIndicatorId iid ++ ")"

renderWrappedTextField :: String -> T.Text -> [String]
renderWrappedTextField field value =
    case wrapWords 58 (T.words value) of
        [] -> ["        , " ++ field ++ " = \"\""]
        firstLine : remaining ->
            [ "        , " ++ field ++ " ="
            , "            " ++ show (T.unpack firstLine)
            ] ++
            [ "            <> " ++ show (" " ++ T.unpack line)
            | line <- remaining
            ]

wrapWords :: Int -> [T.Text] -> [T.Text]
wrapWords width = reverse . foldl addWord []
  where
    addWord [] word = [word]
    addWord (line : lines) word
        | T.length line + 1 + T.length word <= width = (line <> " " <> word) : lines
        | otherwise = word : line : lines
