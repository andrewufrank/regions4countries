-----------------------------------------------------------------------------
--
-- Module      :   Config

-- the database and directory where the files are

-----------------------------------------------------------------------------

module Study.Config (filesUsed, dbPath) where

databaseFolder = "/home/frank/afWorldDB/"

databaseNmae = "r4cdb2" 

dbPath = databaseFolder ++ databaseNmae ++ ".sqlite"

filesUsed :: FilePath
filesUsed =
    "/home/frank/Desktop/buecher/worldFundamentals/WorldBankData/filesUsed"