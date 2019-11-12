module Spec where

import Lib
import DeBruijn

{-
main :: IO ()
main = putStrLn "Test suite not yet implemented"
-}

test1 :: Bool
test =
    if (BuildSequence("01", 2) == ["00", "01", "10", "11"])
        then putStrLn("OK")
    else
        putStrLn("KO")

