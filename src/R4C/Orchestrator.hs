-----------------------------------------------------------------------------
--
-- Module      :   Orchestrator
-- the connection between the module reading the WorldBank files 
-- and the database storing  
-----------------------------------------------------------------------------
module R4C.Orchestrator
    ( importWorldBankFiles
    ) where

import Database.SQLite.Simple

import Control.Monad (forM_)

import R4C.WorldBank
import R4C.Database

importWorldBankFiles
    :: FilePath      -- database
    -> [FilePath]    -- World Bank CSV files
    -> IO ()
importWorldBankFiles dbName files = do

    conn <- open dbName

    createSchema conn

    withTransaction conn $
        forM_ files (importOneIndicatorFile conn)

    close conn

------------------------------------------------------------

importOneIndicatorFile
    :: Connection
    -> FilePath
    -> IO ()
importOneIndicatorFile conn file = do

    putStrLn ("Importing " ++ file)

    (observations) <- readIndicatorFile file

    -- insertIndicator conn indicator
    insertObservations conn observations

    putStrLn $
        "  imported "
        ++ show (length observations)
        ++ " observations"
    
importIndicatorFile
    :: Connection
    -> FilePath
    -> IO ()
importIndicatorFile conn file = do

    (_indicator, observations) <-
        readIndicatorFile file

    insertObservations conn observations
    countries <- readCountryMetadataFile countryPath
    return ()

