{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Parser.Type
Description : ソースコードを木に変換する
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

ソースコードを木構造に変換する。
-}

module Parser.Type
    ( Expr(..)
    , Term(..)
    , Type
    , SourcePos(..)
    , sourcePosPretty
    , getPos
    , mkPos
    ) where

import Text.Megaparsec hiding (label, Label)
import Prelude hiding (div) 

data Expr = Define { exprPos  :: SourcePos
                   , name :: String
                   , args :: [String]
                   , body :: Term } 
          | TypeDef { exprPos  :: SourcePos
                    , name :: String
                    , args :: [String]
                    } deriving Eq

data Term = Add    { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | Sub    { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | Mul    { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | Div    { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | App    { pos :: SourcePos, term :: Term, terms :: [Term] }
          | Eq     { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | Ne     { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | Gt     { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | Lt     { pos :: SourcePos, term1 :: Term, term2 :: Term }
          | If     { pos :: SourcePos, cond :: Term, term1 :: Term, term2 ::Term }
          | True   { pos :: SourcePos }
          | False  { pos :: SourcePos }
          | Num    { pos :: SourcePos, num ::  Int }
          | Label  { pos :: SourcePos, label :: String }
          | Let    { pos :: SourcePos, exprs :: [Expr], term :: Term }
          | Lambda { pos :: SourcePos, label :: String, term :: Term }
          deriving Eq

type Type = String

instance Show Expr where
    show (Define _ n as b) = "Define " ++ n ++ " " ++ show as ++ " " ++ show b
    show (TypeDef _ n as)  = "TypeDef " ++ n ++ " " ++ show as

instance Show Term where
    show Add{..} = "Add " ++ show term1 ++ " " ++ show term2
    show Sub{..} = "Sub " ++ show term1 ++ " " ++ show term2
    show Mul{..} = "Mul " ++ show term1 ++ " " ++ show term2
    show Div{..} = "Div " ++ show term1 ++ " " ++ show term2
    show App{..} = "App " ++ show term ++ " " ++ show terms
    show Eq {..} = "Eq  " ++ show term1 ++ " " ++ show term2
    show Ne{..}  = "Ne  " ++ show term1 ++ " " ++ show term2
    show Gt{..}  = "Gt  " ++ show term1 ++ " " ++ show term2
    show Lt{..}  = "Lt  " ++ show term1 ++ " " ++ show term2
    show If{..}  = "If  " ++ show cond ++ " " ++ show term1 ++ " " ++ show term2
    show Parser.Type.True{}   = "True"
    show Parser.Type.False{} = "False"
    show Num{..} = show num
    show Label{..} = label
    show Let{..}  = "Let " ++ show exprs ++ show term
    show Lambda{..} = "\\" ++ label ++ " -> " ++ show term

getPos :: Term -> SourcePos
getPos (Add   p _ _)    = p
getPos (Sub   p _ _)    = p
getPos (Mul   p _ _)    = p
getPos (Div   p _ _)    = p
getPos (App   p _ _)    = p
getPos (Eq    p _ _)    = p
getPos (Ne    p _ _)    = p
getPos (Gt    p _ _)    = p
getPos (Lt    p _ _)    = p
getPos (If    p _ _ _)  = p
getPos (Parser.Type.True  p) = p
getPos (Parser.Type.False p) = p
getPos (Num   p _)      = p
getPos (Label p _)      = p
getPos (Let   p _ _)    = p
getPos (Lambda p _ _)   = p

