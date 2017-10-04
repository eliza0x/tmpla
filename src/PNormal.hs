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
            pos    :: P.SourcePos 
          , name   :: String 
          , body   :: Term 
          } deriving Eq

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
          | Lambda P.SourcePos String Type Term    
          | App    P.SourcePos Term Term    
          deriving Eq

instance Show Expr where
    show (Define _ n b) = "Define " ++ n ++ " " ++ show b

instance Show Term where
    show (Add _ t t')     = "Add " ++ show t ++ " " ++ show t'
    show (Sub _ t t')     = "Sub " ++ show t ++ " " ++ show t'
    show (Mul _ t t')     = "Mul " ++ show t ++ " " ++ show t'
    show (Div _ t t')     = "Div " ++ show t ++ " " ++ show t'
    show (App _ t t')     = "App " ++ show t ++ " " ++ show t'
    show (Eq  _ t t')     = "Eq  " ++ show t ++ " " ++ show t'
    show (Ne  _ t t')     = "Ne  " ++ show t ++ " " ++ show t'
    show (Gt  _ t t')     = "Gt  " ++ show t ++ " " ++ show t'
    show (Lt  _ t t')     = "Lt  " ++ show t ++ " " ++ show t'
    show (If  _ b t t')   = "If  " ++ show b ++ " " ++ show t ++ " " ++ show t'
    show (PNormal.True  _) = "True"
    show (PNormal.False _) = "False"
    show (Num _ n)        = show n
    show (Label _ l)      = l
    show (Let _ exprs t)  = "Let " ++ show exprs ++ " in " ++ show t
    show (Lambda _ l _ tr)   = "\\" ++ show l ++ " -> " ++ show tr

type Type = String
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
    P.Lambda p l t    -> Lambda p l "Unknown" (preNormTerm t)
    P.App    p t ts   -> foldl (App p) (preNormTerm t) (map preNormTerm ts)   

