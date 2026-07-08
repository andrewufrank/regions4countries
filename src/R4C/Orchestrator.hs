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
        forM_ files (importOneFile conn)

    close conn

------------------------------------------------------------

importOneFile
    :: Connection
    -> FilePath
    -> IO ()
importOneFile conn file = do

    putStrLn ("Importing " ++ file)

    (indicator, observations) <- importFile file

    insertIndicator conn indicator
    insertObservations conn observations

    putStrLn $
        "  imported "
        ++ show (length observations)
        ++ " observations"

testorchestrator = do 
     importWorldBankFiles
        "test.sqlite"
        [ "/home/frank/Desktop/buecher/nextOrder/WorldBankData/population/f27274b4-7384-4c6e-b81d-7ddf2ac9bb9a_Data.csv"
        , "/home/frank/Desktop/buecher/nextOrder/WorldBankData/surfaceArea/API_AG.SRF.TOTL.K2_DS2_en_csv_v2_4649.csv"
        ]
