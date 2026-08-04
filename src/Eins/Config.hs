-----------------------------------------------------------------------------
--
-- Module      :   Config Study

-- the database and directory where the files are

-----------------------------------------------------------------------------

module Eins.Config where

databaseFolder = "/home/frank/afWorldDB/"

databaseNmae = "r4cdb6"

dbPath = databaseFolder ++ databaseNmae ++ ".sqlite"

filesUsed :: FilePath
-- for loading WorldBank data
filesUsed =
    "/home/frank/Desktop/buecher/wfData/WorldBankData/"

energyInstituteFile :: FilePath
-- the compact cvs file with all the data 
energyInstituteFile =
    "/home/frank/Desktop/buecher/wfData/EnergyInstituteData/Statistical Review of World Energy Narrow format.csv"

buch :: FilePath
buch = "/home/frank/Desktop/buecher/worldFundamentals"

tableOutputDirectory :: FilePath
tableOutputDirectory =
    "/home/frank/Desktop/buecher/worldFundamentals/DNB_figures"
