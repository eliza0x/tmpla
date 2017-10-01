module Main where

import Parser
import PNormal
import Type
-- import KNormal
-- import Emitter

import System.Environment (getArgs)
import Data.Either (isRight)
-- import Control.Monad (when)
import qualified Control.Eff as E
import qualified Control.Eff.Exception as EE

main :: IO ()
main = do
    arg <- head <$> getArgs
    file <- readFile arg
    let exprs = parser arg file
    putStrLn "Source code"
    putStrLn file
    putStrLn "---------------"
    putStrLn "Parse"
    mapM_ print exprs
    putStrLn "---------------"
    putStrLn "PNormal"
    let pnorm = pnormalize exprs
    print pnorm
    putStrLn "---------------"
    putStrLn "Type Check"
    let typed = E.run . EE.runExc $ typeCheck pnorm
    putStrLn $ if isRight typed then "OK" else error $ show typed
    -- when typed $ do
    --     putStrLn "---------------"
    --     putStrLn "KNormal\n"
    --     knorms <- knormalize exprs
    --     putStrLn . unlines . map show $ knorms

