-----------------------------------------------------------------------------
--
-- Module      :   Orchestrator
-- the connection between the module reading the WorldBank files
-- and the database storing
-----------------------------------------------------------------------------
module R4C.Import.Orchestrator (
    importWorldBankArchives,
    importEnergyInstitute2025,
) where

import Database.SQLite.Simple

import Control.Monad (forM_)

import R4C.Import.Database
import R4C.Import.EnergyInstitute
import R4C.Import.WorldBank
import R4C.Model (Year (..))
import UniformBase

importWorldBankArchives ::
    FilePath -> -- database
    [FilePath] -> -- World Bank CSV archives
    IO ()
importWorldBankArchives dbName files = do
    conn <- open dbName

    createSchema conn

    withTransaction conn $
        forM_ files (importOneArchive conn)

    close conn

------------------------------------------------------------
importOneArchive ::
    Connection ->
    FilePath ->
    IO ()
importOneArchive conn archiveFile = do
    archive <-
        readArchive archiveFile

    insertIndicator
        conn
        (archiveIndicator archive)

    insertCountries
        conn
        (archiveCountries archive)

    insertObservations
        conn
        (archiveObservations archive)

importEnergyInstitute2025 :: FilePath -> FilePath -> IO ()
importEnergyInstitute2025 dbName csvFile = do
    conn <- open dbName
    createSchema conn
    rows <- readEnergyInstituteObservations csvFile (Year 2025)
    withTransaction conn $ do
        mapM_ (insertIndicator conn) energyInstituteIndicators
        insertObservations conn rows
    close conn

-- importOneIndicatorFile
--     :: Connection
--     -> FilePath
--     -> IO ()
-- importOneIndicatorFile conn file = do

--     putStrLn ("Importing " ++ file)

--     (observations) <- readIndicatorFile file

--     -- insertIndicator conn indicator
--     insertObservations conn observations

--     putStrLn $
--         "  imported "
--         ++ show (length observations)
--         ++ " observations"

-- importIndicatorFile
--     :: Connection
--     -> FilePath
--     -> IO ()
-- importIndicatorFile conn file = do

--     (_indicator, observations) <-
--         readIndicatorFile file

--     insertObservations conn observations
--     countries <- readCountryMetadataFile countryPath
--     return ()
