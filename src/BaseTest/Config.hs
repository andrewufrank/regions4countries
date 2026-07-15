-----------------------------------------------------------------------------
--
-- Module      :   Config

-- the database and directory where the files are

-----------------------------------------------------------------------------

module BaseTest.Config (filesUsed, dbPath) where

databaseFolder = "/home/frank/afWorldDB/"

databaseNmae = "r4ctestdb4" 

dbPath = databaseFolder ++ databaseNmae ++ ".sqlite"

filesUsed :: FilePath
filesUsed =
    "/home/frank/Desktop/buecher/WorldBankData/archivesUsed"
    