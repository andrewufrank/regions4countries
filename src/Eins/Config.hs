-----------------------------------------------------------------------------
--
-- Module      :   Config Study

-- the database and directory where the files are

-----------------------------------------------------------------------------

module Eins.Config  where

databaseFolder = "/home/frank/afWorldDB/"

databaseNmae = "r4cdb6" 

dbPath = databaseFolder ++ databaseNmae ++ ".sqlite"

filesUsed :: FilePath
filesUsed =
    "/home/frank/Desktop/buecher/WorldBankData/archivesUsed"

buch:: FilePath
buch = "/home/frank/Desktop/buecher/worldFundamentals"

tableOutputDirectory :: FilePath
tableOutputDirectory =
    "/home/frank/Desktop/buecher/worldFundamentals/figures"