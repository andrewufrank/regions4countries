-----------------------------------------------------------------------------
--
-- Module      :   Config

-- the database and directory where the files are

-----------------------------------------------------------------------------

module BaseTest.Config   where

databaseFolder = "/home/frank/afWorldDB/"

databaseNmae = "r4cdb4aTest" 

dbPath = databaseFolder ++ databaseNmae ++ ".sqlite"

filesUsed :: FilePath
filesUsed =
    "/home/frank/Desktop/buecher/WorldBankData/archivesUsed"

buch= "/home/frank/Workspace12/regions4countries/test/testbuch"