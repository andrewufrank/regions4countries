-- | Compare the checked-in study registry with World Bank metadata in SQLite.
module Main where

import Control.Exception (bracket)
import Data.List (isPrefixOf, sortOn)
import System.Directory (renameFile)
import System.Environment (getArgs)
import System.IO (hClose, hPutStr, openTempFile)

import R4C.Import.Database (closeDB, indicators4db, openDB)
import R4C.Model
    ( Dataset, dsIndicator, dsName, dsDefinition, dsSourceOrganization )
import           Study.Descriptor  (namedDatasets)
import           Study.Descriptor2
    ( DatasetWarning(..), reconcileDatasets, renderDatasetTemplate
    , renderNamedDataset, renderWarning )
import Study.Config

main :: IO ()
main = do
    args <- getArgs
    writeSource <- case args of
        [] -> pure False
        ["--write"] -> pure True
        _ -> error "Usage: cabal run sync-datasets -- [--write]"

    bracket (openDB dbPath) closeDB $ \conn -> do
        imported <- indicators4db conn
        let configured = map snd namedDatasets
            (warnings, updated, newDatasets) = reconcileDatasets configured imported
            safeUpdated = zipWith (preserveConflicts warnings) namedDatasets updated

        mapM_ (putStrLn . renderWarning) warnings
        if writeSource
            then do
                rewriteDatasetSource safeUpdated
                putStrLn "Updated non-conflicting metadata in src/Study/Dataset.hs."
            else pure ()
        if null newDatasets
            then pure ()
            else do
                putStrLn "\nNew Dataset templates (review every TODO before adding them):"
                mapM_ (putStrLn . renderDatasetTemplate) newDatasets

preserveConflicts :: [DatasetWarning] -> (String, Dataset) -> Dataset -> (String, Dataset)
preserveConflicts warnings (name, original) refreshed
    = (name, refreshed
        { dsName = keep "name" (dsName original) (dsName refreshed)
        , dsDefinition = keep "definition" (dsDefinition original) (dsDefinition refreshed)
        , dsSourceOrganization =
            keep "source organization"
                (dsSourceOrganization original)
                (dsSourceOrganization refreshed)
        })
  where
    keep field old new
        | any (conflictsWith (dsIndicator original) field) warnings = old
        | otherwise = new

    conflictsWith iid field (MetadataChanged warningId warningField _ _) =
        iid == warningId && field == warningField
    conflictsWith _ _ _ = False

rewriteDatasetSource :: [(String, Dataset)] -> IO ()
rewriteDatasetSource records = do
    let path = "src/Study/Dataset.hs"
        begin = "-- BEGIN GENERATED DATASETS"
        end = "-- END GENERATED DATASETS"
        generated = unlines
            (map renderNamedDataset (sortOn (dsIndicator . snd) records))
    source <- readFile path
    case replaceSection begin end generated source of
        Nothing -> error "sync-datasets: generated Dataset section not found"
        Just rewritten -> do
            (temporaryPath, handle) <- openTempFile "src/Study" "Dataset2.hs.sync"
            hPutStr handle rewritten
            hClose handle
            renameFile temporaryPath path

replaceSection :: String -> String -> String -> String -> Maybe String
replaceSection begin end replacement source = do
    (before, fromBegin) <- breakOn begin source
    let afterBegin = drop (length begin) fromBegin
    (_, fromEnd) <- breakOn end afterBegin
    let afterEnd = drop (length end) fromEnd
    pure (before ++ begin ++ "\n" ++ replacement ++ end ++ afterEnd)

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go []
  where
    go _ [] = Nothing
    go before remaining@(next : rest)
        | needle `isPrefixOf` remaining = Just (reverse before, remaining)
        | otherwise = go (next : before) rest
