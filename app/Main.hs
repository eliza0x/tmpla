module Main where

import Parser
import Type
import KNormal
-- import Emitter

import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
    arg <- head <$> getArgs
    file <- readFile arg
    let exprs = parser arg file
    putStrLn "Parse"
    mapM_ print exprs
    putStrLn "---------------"
    putStrLn "Type Check"
    let typed = typeCheck exprs
    putStrLn $ if typed then "OK" else "Bad"
    when typed $ do
        putStrLn "---------------"
        putStrLn "KNormal\n"
        knorms <- knormalize exprs
        putStrLn . unlines . map show $ knorms

