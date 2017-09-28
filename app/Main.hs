module Main where

import Parser
import KNormal
-- import Emitter

main :: IO ()
main = do
    file <- getContents
    let exprs = parser file
    knorms <- knormalize exprs 
    putStrLn . unlines $ map show knorms

