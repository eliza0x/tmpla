module Alpha
    ( alpha
    , Env
    ) where

import qualified PNormal as P
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
alphaT prefix env term = case term of
    P.Add    p t t'    -> P.Add    p (alphaT prefix env t) (alphaT prefix env t')    
    P.Sub    p t t'    -> P.Sub    p (alphaT prefix env t) (alphaT prefix env t')    
    P.Mul    p t t'    -> P.Mul    p (alphaT prefix env t) (alphaT prefix env t')    
    P.Div    p t t'    -> P.Div    p (alphaT prefix env t) (alphaT prefix env t')    
    P.Eq     p t t'    -> P.Eq     p (alphaT prefix env t) (alphaT prefix env t')    
    P.Ne     p t t'    -> P.Ne     p (alphaT prefix env t) (alphaT prefix env t')    
    P.Gt     p t t'    -> P.Gt     p (alphaT prefix env t) (alphaT prefix env t')    
    P.Lt     p t t'    -> P.Lt     p (alphaT prefix env t) (alphaT prefix env t')    
    P.If     p b t t'  -> P.If     p (alphaT prefix env b) (alphaT prefix env t) (alphaT prefix env t')
    P.True   p         -> P.True   p 
    P.False  p         -> P.False  p       
    P.Num    p n       -> P.Num    p n       
    P.Label  p l       -> P.Label  p 
                        . fromMaybe (error "[INTERNAL ERROR] Alpha.hs alphaT") 
                        $ M.lookup l env
    P.Lambda p l ty tr -> P.Lambda p (prefix' l) ty $ alphaT (prefix' l) (M.insert l (prefix' l) env) tr
    P.App    p t t'    -> P.App p (alphaT prefix env t) (alphaT prefix env t')
    P.Let    p es t    -> let
        env' = inspectEnv prefix env es
        in P.Let p (alpha' prefix env' es) (alphaT prefix env' t)
    where 
    prefix' n = if null prefix then n else prefix ++ "#" ++ n

inspectEnv :: String -> Env -> [P.Expr] -> Env
inspectEnv prefix =  foldr (\expr env' -> M.insert (P.name expr) (prefix' $ P.name expr) env') 
    where 
    prefix' n = if null prefix then n else prefix ++ "#" ++ n


