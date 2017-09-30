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

data Expr = Define {
            pos  :: P.SourcePos 
          , name :: String 
          , body :: Term 
          }
          | TypeDef {
            pos  :: P.SourcePos 
          , name :: String 
          , args :: [String] 
          } deriving (Show, Eq)

data Term = Add    P.SourcePos Term Term  
          | Sub    P.SourcePos Term Term    
          | Mul    P.SourcePos Term Term    
          | Div    P.SourcePos Term Term    
          | Eq     P.SourcePos Term Term     
          | Ne     P.SourcePos Term Term     
          | Gt     P.SourcePos Term Term     
          | Lt     P.SourcePos Term Term     
          | If     P.SourcePos Term Term Term    
          | True   P.SourcePos     
          | False  P.SourcePos    
          | Num    P.SourcePos Int  
          | Label  P.SourcePos String     
          | Let    P.SourcePos [Expr] Term  
          | Lambda P.SourcePos String Term    
          | App    P.SourcePos String [Term]    
          deriving (Show, Eq)

pnormalize :: [P.Expr] -> [Expr]
pnormalize = map pnorm
    where
    pnorm :: P.Expr -> Expr
    pnorm (P.TypeDef p n as) = TypeDef p n as
    pnorm (P.Define p n as b) = Define p n $ foldr (Lambda p) (preNormTerm b) as

preNormTerm :: P.Term -> Term
preNormTerm term = case term of
    P.Add    p t t'   -> Add    p (preNormTerm t) (preNormTerm t')
    P.Sub    p t t'   -> Sub    p (preNormTerm t) (preNormTerm t')   
    P.Mul    p t t'   -> Mul    p (preNormTerm t) (preNormTerm t')   
    P.Div    p t t'   -> Div    p (preNormTerm t) (preNormTerm t')   
    P.Eq     p t t'   -> Eq     p (preNormTerm t) (preNormTerm t')      
    P.Ne     p t t'   -> Ne     p (preNormTerm t) (preNormTerm t')      
    P.Gt     p t t'   -> Gt     p (preNormTerm t) (preNormTerm t')      
    P.Lt     p t t'   -> Lt     p (preNormTerm t) (preNormTerm t')      
    P.If     p b t t' -> If     p (preNormTerm b) (preNormTerm t)  (preNormTerm t')
    P.True   p        -> PNormal.True  p
    P.False  p        -> PNormal.False p      
    P.Num    p n      -> Num    p n      
    P.Label  p l      -> Label  p l       
    P.Let    p es t   -> Let    p (pnormalize es) (preNormTerm t)
    P.Lambda p l t    -> Lambda p l (preNormTerm t)
    P.App    p l ts   -> App    p l (map preNormTerm ts)   
    

