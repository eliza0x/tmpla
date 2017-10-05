{-# LANGUAGE TypeOperators #-}

module Main where

import Parser
import PNormal
import Type
import Alpha
import KNormal
-- import Emitter

import System.Environment (getArgs)
import Data.Either (isRight)
import Control.Monad (when)
import qualified Control.Eff as E
import qualified Control.Eff.Exception as EE
import Control.Eff ((:>))
import Data.Void (Void)

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
    putStrLn "Alpha"
    let al = alpha pnorm
    print al
    putStrLn "---------------"
    putStrLn "Type Check"
    let typed = E.run . EE.runExc $ (typeCheck al :: E.Eff (EE.Exc TypeCheckError :> Void) Env)
    print typed
    when (isRight typed) $ do
        putStrLn "---------------"
        putStrLn "KNormal\n"
        knorms <- knormalize al
        putStrLn . unlines . map show $ knorms

