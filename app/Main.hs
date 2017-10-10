{-# LANGUAGE TypeOperators #-}

module Main where

import Parser
import SyntaxSugar
import Typeing
import Alpha
import KNormal
import Expansion
import Asm

import System.Environment (getArgs)
import Data.Either (isRight)
import Control.Monad (when)
import qualified Control.Eff as E
import qualified Control.Eff.Exception as EE
import Control.Eff ((:>))
import Data.Void (Void)

main :: IO ()
main = do
    fileName <- head <$> getArgs
    file <- readFile fileName
    let parsed = parser fileName file
    putStrLn "Source code"
    putStrLn file
    putStrLn "---------------"
    putStrLn "Parse"
    mapM_ print parsed
    putStrLn "---------------"
    putStrLn "PNormal"
    let pnorm = pnormalize parsed
    print pnorm
    putStrLn "---------------"
    putStrLn "Alpha"
    let al = alpha pnorm
    print al
    putStrLn "---------------"
    putStrLn "Type Check"
    let typed = E.run . EE.runExc $ (typeCheck al :: E.Eff (EE.Exc TypeCheckError :> Void) Typeing.Env)
    print typed
    when (isRight typed) $ do
        putStrLn "---------------"
        putStrLn "KNormal\n"
        knorms <- knormalize al
        putStrLn . unlines . map show $ knorms
        putStrLn "---------------"
        putStrLn "ANormal\n"
        anorms <- anormalize knorms
        putStrLn . unlines . map show $ anorms
        putStrLn "---------------"
        putStrLn "Emit\n"
        mapM_ print $ emit anorms
