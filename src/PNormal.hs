{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : PNormal
Description : 型検査をする
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

事前処理をする
1. 関数をラムダ式に展開
2. where式をlet文に展開(予定)
-}

module PNormal
    ( pnormalize
    , Expr(..)
    , Term(..)
    ) where

import qualified Parser as P
import qualified Data.Map.Lazy as M
import qualified Data.Maybe as M

data Expr = Define {
            exprPos    :: P.SourcePos 
          , name   :: String 
          , body   :: Term 
          } deriving Eq

data Term = Add    { pos :: P.SourcePos, term1 :: Term, term2 :: Term}  
          | Sub    { pos :: P.SourcePos, term1 :: Term, term2 :: Term}    
          | Mul    { pos :: P.SourcePos, term1 :: Term, term2 :: Term}    
          | Div    { pos :: P.SourcePos, term1 :: Term, term2 :: Term}    
          | Eq     { pos :: P.SourcePos, term1 :: Term, term2 :: Term}     
          | Ne     { pos :: P.SourcePos, term1 :: Term, term2 :: Term}     
          | Gt     { pos :: P.SourcePos, term1 :: Term, term2 :: Term}     
          | Lt     { pos :: P.SourcePos, term1 :: Term, term2 :: Term}     
          | If     { pos :: P.SourcePos, cond :: Term, term1 :: Term, term2 :: Term}
          | App    { pos :: P.SourcePos, term1 :: Term, term2 :: Term}
          | True   { pos :: P.SourcePos }    
          | False  { pos :: P.SourcePos }   
          | Num    { pos :: P.SourcePos, num :: Int }
          | Label  { pos :: P.SourcePos, label :: String }
          | Let    { pos :: P.SourcePos, exprs :: [Expr], term :: Term }
          | Lambda { pos :: P.SourcePos, label :: String, arg :: Type, term :: Term}
          deriving Eq

type Type = String

instance Show Expr where
    show Define{..} = "Define " ++ name ++ " " ++ show body

instance Show Term where
    show Add{..} = "Add " ++ show term1 ++ " " ++ show term2
    show Sub{..} = "Sub " ++ show term1 ++ " " ++ show term2
    show Mul{..} = "Mul " ++ show term1 ++ " " ++ show term2
    show Div{..} = "Div " ++ show term1 ++ " " ++ show term2
    show Eq{..}  = "Eq  " ++ show term1 ++ " " ++ show term2
    show Ne{..}  = "Ne  " ++ show term1 ++ " " ++ show term2
    show Gt{..}  = "Gt  " ++ show term1 ++ " " ++ show term2
    show Lt{..}  = "Lt  " ++ show term1 ++ " " ++ show term2
    show App{..} = "App " ++ show term1 ++ " " ++ show term2
    show If{..}  = "If  " ++ show cond ++ " " ++ show term1 ++ " " ++ show term2
    show Num{..}    = show num
    show Label{..}  = label
    show Let{..}    = "Let " ++ show exprs ++ " in " ++ show term
    show Lambda{..} = "\\" ++ show label ++ " -> " ++ show term
    show PNormal.True{}  = "True"
    show PNormal.False{} = "False"

type Env = M.Map String [Type]

pnormalize :: [P.Expr] -> [Expr]
pnormalize exprs = map (pnorm (foldr genEnv M.empty $ filter (not . isDefine) exprs))
                       (filter isDefine exprs)
    where
    isDefine :: P.Expr -> Bool
    isDefine P.Define{} = Prelude.True
    isDefine P.TypeDef{} = Prelude.False

    genEnv :: P.Expr -> Env -> Env
    genEnv (P.TypeDef _ n as) = M.insert n as
    genEnv _                  = undefined

    pnorm :: Env -> P.Expr -> Expr
    pnorm env (P.Define p n as b) = let 
        types :: [Type]
        types = M.fromMaybe (replicate (length as) "Unknown") $ M.lookup n env
        in Define p n
         $ foldr ($) (preNormTerm b)
         $ zipWith (Lambda p) as types
    pnorm _ _ = undefined

preNormTerm :: P.Term -> Term
preNormTerm = \case
    P.Add   {..} -> Add    pos (preNormTerm term1) (preNormTerm term2)
    P.Sub   {..} -> Sub    pos (preNormTerm term1) (preNormTerm term2)   
    P.Mul   {..} -> Mul    pos (preNormTerm term1) (preNormTerm term2)   
    P.Div   {..} -> Div    pos (preNormTerm term1) (preNormTerm term2)   
    P.Eq    {..} -> Eq     pos (preNormTerm term1) (preNormTerm term2)      
    P.Ne    {..} -> Ne     pos (preNormTerm term1) (preNormTerm term2)      
    P.Gt    {..} -> Gt     pos (preNormTerm term1) (preNormTerm term2)      
    P.Lt    {..} -> Lt     pos (preNormTerm term1) (preNormTerm term2)      
    P.If    {..} -> If     pos (preNormTerm cond) (preNormTerm term1) (preNormTerm term2)
    P.True  {..} -> PNormal.True  pos
    P.False {..} -> PNormal.False pos    
    P.Num   {..} -> Num    pos num      
    P.Label {..} -> Label  pos label     
    P.Let   {..} -> Let    pos (pnormalize exprs) (preNormTerm term)
    P.Lambda{..} -> Lambda pos label "Unknown" (preNormTerm term)
    P.App   {..} -> foldl (App pos) (preNormTerm term) (map preNormTerm terms)

