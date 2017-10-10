{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Alpha
    ( alpha
    , Env
    ) where

import qualified SyntaxSugar as P
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)

alpha :: [P.Expr] -> [P.Expr]
alpha = alpha' "" M.empty

alpha' :: String -> Env -> [P.Expr] -> [P.Expr]
alpha' prefix env exprs = map (alphaExpr prefix (inspectEnv prefix env exprs)) exprs

type Env = M.Map String String

alphaExpr :: String -> Env -> P.Expr -> P.Expr
alphaExpr prefix env (P.Define p n b) = P.Define p prefix'
                                      $ alphaT prefix' (M.insert n prefix' env) b
    where 
    prefix' = if null prefix then n else prefix ++ "#" ++ n

alphaT :: String -> Env -> P.Term -> P.Term
alphaT prefix env = \case
    P.Add    {..} -> P.Add    pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.Sub    {..} -> P.Sub    pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.Mul    {..} -> P.Mul    pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.Div    {..} -> P.Div    pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.Eq     {..} -> P.Eq     pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.Ne     {..} -> P.Ne     pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.Gt     {..} -> P.Gt     pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.Lt     {..} -> P.Lt     pos (alphaT prefix env term1) (alphaT prefix env term2)    
    P.If     {..} -> P.If     pos (alphaT prefix env cond) (alphaT prefix env term1) (alphaT prefix env term2)
    P.True   {..} -> P.True   pos 
    P.False  {..} -> P.False  pos       
    P.Num    {..} -> P.Num    pos num
    P.Label  {..} -> P.Label  pos 
                        . fromMaybe (error "[INTERNAL ERROR] Alpha.hs alphaT") 
                        $ M.lookup label env
    P.Lambda {..} -> P.Lambda pos (prefix' label) arg 
                        $ alphaT (prefix' label) 
                                 (M.insert label (prefix' label) env)
                                 term
    P.App    {..} -> P.App pos (alphaT prefix env term1) (alphaT prefix env term2)
    P.Let    {..} -> let
        env' = inspectEnv prefix env exprs
        in P.Let pos (alpha' prefix env' exprs) (alphaT prefix env' term)
    where 
    prefix' n = if null prefix then n else prefix ++ "#" ++ n

inspectEnv :: String -> Env -> [P.Expr] -> Env
inspectEnv prefix = foldr (\expr env' -> M.insert (P.name expr) (prefix' $ P.name expr) env') 
    where 
    prefix' n = if null prefix then n else prefix ++ "#" ++ n


