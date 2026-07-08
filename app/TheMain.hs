-----------------------------------------------------------------------------
--
-- Module      :   a test  
----------------------------------------------------------------------


module Main     where      -- must have Main (main) or Main where

 
import UniformBase

main :: IO ()
main =  do  -- with tests in other modules
    startProg "The Main Example" 
        (do 
            putIOwords ["The Main of Example"]
            )
    -- dirMain
    -- openMain
    return ()

