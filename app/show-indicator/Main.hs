module Main where

import R4C.Import.Database (openDB, lookupTable, indicators4db)
import R4C.Model (IndicatorId(..), Year(..), CountryValue(..), CountryId(..), Indicator(..))

import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Control.Exception (bracket)
import Database.SQLite.Simple (Only(..), query)
import R4C.Import.Database (closeDB)
import Data.Foldable (for_)
import Study.Config
import qualified Data.Text as T

main :: IO ()
main = do
     showAllIndicators dbPath 

showAllIndicators :: FilePath -> IO ()
showAllIndicators dbPath =
    bracket (openDB dbPath) closeDB $ \conn -> do
        inds <- indicators4db conn
        mapM_ showIndicator inds



showIndicator :: Indicator -> IO ()
showIndicator ind = do
    putStrLn $ "ID:                  "
            ++ T.unpack (unIndicatorId (indicatorId ind))
    putStrLn $ "Name:                "
            ++ T.unpack (indicatorName ind)
    putStrLn $ "Source note:         "
            ++ T.unpack (sourceNote ind)
    putStrLn $ "Source organization: "
            ++ T.unpack (sourceOrganization ind)
    putStrLn ""
