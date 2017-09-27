module Main where

import Parser
import Emitter

import System.Environment

main :: IO ()
main = do
    file <- getContents
    print $ parser file

