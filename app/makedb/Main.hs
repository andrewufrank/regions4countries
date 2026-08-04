module Main where

import Control.Monad (when)
import Data.List (sort)
import Eins.Config
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeFile,
 )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (
    takeExtension,
    (</>),
 )

import R4C.Import.Orchestrator

main :: IO ()
main = do
    csvPaths <- findZipFiles filesUsed
    when (null csvPaths) $
        die $
            "makedb: no Zip archives found in " ++ filesUsed

    removeOldDatabase dbPath

    putStrLn $ "Creating database: " ++ dbPath
    -- is implied in importWorldArchives

    putStrLn $
        " and Loading WorldBank archives from: "
            ++ filesUsed

    mapM_ printInputFile csvPaths

    putStrLn "start loading"
    importWorldBankArchives dbPath csvPaths

    putStrLn $
        "Loading selected Energy Institute datasets from: "
            ++ energyInstituteFile
    importEnergyInstitute2025 dbPath energyInstituteFile

    putStrLn $
        "Imported " ++ show (length csvPaths) ++ " World Bank file(s)"
    putStrLn "Database creation complete."

-- findCsvFiles :: FilePath -> IO [FilePath]
findZipFiles directory = do
    exists <- doesDirectoryExist directory

    when (not exists) $
        die $
            "makedb: directory does not exist: " ++ directory

    names <- listDirectory directory

    pure
        [ directory </> name
        | name <- sort names
        , takeExtension name == ".zip"
        ]

printInputFile :: FilePath -> IO ()
printInputFile path =
    putStrLn $ "    " ++ path

removeOldDatabase :: FilePath -> IO ()
removeOldDatabase path = do
    exists <- doesFileExist path

    when exists $ do
        putStrLn $ "Removing existing database: " ++ path
        removeFile path

-- usage :: String
-- usage =
--     unlines
--         [ "Usage:"
--         , "    makedb DATABASE"
--         , ""
--         , "All CSV files in the directory 'filesUsed' are imported."
--         , ""
--         , "Example:"
--         , "    cabal run makedb -- data/r4c.sqlite"
--         ]
