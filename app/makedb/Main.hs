module Main where

import Control.Monad (when)
import Data.List (sort)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removeFile
    )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
    ( takeExtension
    , (</>)
    )
import Study.Config

import R4C.Orchestrator  


main :: IO ()
main = do
    csvPaths <- findCsvFiles filesUsed
    when (null csvPaths) $
        die $ "makedb: no CSV files found in " ++ filesUsed

    removeOldDatabase dbPath

    putStrLn $ "Creating database: " ++ dbPath
    putStrLn $ "Loading CSV files from: " ++ "/home/frank/Desktop/buecher/nextOrder/WorldBankData/filesUsed"

    mapM_ printInputFile csvPaths

    putStrLn "start loading"
    importWorldBankFiles dbPath csvPaths

    putStrLn $ "Imported " ++ show (length csvPaths) ++ " file(s)"
    putStrLn "Database creation complete."


findCsvFiles :: FilePath -> IO [FilePath]
findCsvFiles directory = do
    exists <- doesDirectoryExist directory

    when (not exists) $
        die $ "makedb: directory does not exist: " ++ directory

    names <- listDirectory directory

    pure
        [ directory </> name
        | name <- sort names
        , takeExtension name == ".csv"
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