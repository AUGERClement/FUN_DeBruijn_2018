module Main where

import Lib
import Parser
import DeBruijn
import System.Environment
import System.Exit
{-From alban piron-}

main :: IO ()
main = do
    args <- getArgs
    case (parse args) of
        Nothing -> do
            putStrLn usage
            exitWith (ExitFailure 84)
        Just (Check int str) -> printBool (fullCheck str int)
        Just (Unique int str) -> printBool (fullUnique str int)
        Just (Clean int str) -> fullClean str int

usage = "USAGE: ./deBruijn n [a] [--check | --unique | clean]\n" ++
        "\t--check\t\tcheck if a sequence is a DeBruijn sequence\n" ++
        "\t--unique\tcheck if 2 sequences are distinct de Bruijn sequence\n" ++
        "\t--clean\t\tlist cleaning\n" ++
        "\tn\t\torder of the sequence\n" ++
        "\ta\t\talphabet [def: \"01\"]"

printBool :: IO Bool -> IO()
printBool result = do
    cast <- result
    if (cast)
        then putStrLn("OK")
    else
        putStrLn("KO")

printArr :: [String] -> IO()
printArr [] = return()
printArr (x:xs) = do
    putStrLn x
    printArr xs

fullCheck :: String -> Int -> IO Bool
fullCheck [] nb = return False
fullCheck str 0 = return False
fullCheck str nb = do
    seq <- getLine
    return (checkFlag str nb seq)

fullUnique :: String -> Int -> IO Bool
fullUnique [] nb = return False
fullUnique str 0 = return False
fullUnique str nb = do
    str1 <- getLine
    str2 <- getLine
    return (uniqueDeBruijn (buildSequence str nb) str1 str2)

getArr :: [String] -> IO [String]
getArr arr = do
    a <- getLine
    if (a == "END")
        then return (reverse arr)
        else getArr (a:arr)

fullClean :: String -> Int -> IO()
fullClean [] nb = return()
fullClean str 0 = return()
fullClean str nb = do
    arr_to_clean <- getArr []
    printArr (cleanFlag str nb arr_to_clean)
