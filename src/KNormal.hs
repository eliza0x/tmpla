{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : KNormal
Description : 木構造の式を展開する
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

木構造の式を展開する、第一段階の中間表現
-}

module KNormal(
      KBlock(..)
    , KNormal(..)
    , NameTag(..)
    , Var
    , Label
    , knormalize
    ) where

import Control.Monad.Writer.Lazy
import qualified Data.List as L

import qualified Parser as P
import qualified Util as U

type Var = String

type Label = String

data KBlock = KBlock 
    { pos  :: P.SourcePos
    , name :: Label
    , args :: [Label]
    , body :: [KNormal]
    } deriving Eq

data KNormal =
      Let   P.SourcePos Var [KBlock] [KNormal]
    | If    P.SourcePos Var [KNormal] [KNormal] [KNormal]
    | Add   P.SourcePos Var Var Var
    | Sub   P.SourcePos Var Var Var 
    | Mul   P.SourcePos Var Var Var
    | Div   P.SourcePos Var Var Var
    | Eq    P.SourcePos Var Var Var
    | Ne    P.SourcePos Var Var Var
    | Gt    P.SourcePos Var Var Var
    | Lt    P.SourcePos Var Var Var
    | App   P.SourcePos Var Label [Var]
    | Call  P.SourcePos Var Label 
    | True  P.SourcePos Var
    | False P.SourcePos Var
    | Num   P.SourcePos Var Int
    | Label P.SourcePos Var Label
    deriving Eq

instance Show KBlock where
    show (KBlock _ n as b) = n ++ " " ++ unwords as ++ " {\n" ++ unlines (map (\x->tab++show x) b) ++ "}\n"

tab :: String
tab = "    "

instance Show KNormal where
    show (Add   _ result var var') = concat [t5 result, " := ", t5 var, " + ",  t5 var', ";"]
    show (Sub   _ result var var') = concat [t5 result, " := ", t5 var, " - ",  t5 var', ";"]
    show (Mul   _ result var var') = concat [t5 result, " := ", t5 var, " * ",  t5 var', ";"]
    show (Div   _ result var var') = concat [t5 result, " := ", t5 var, " / ",  t5 var', ";"]
    show (Eq    _ result var var') = concat [t5 result, " := ", t5 var, " = ",  t5 var', ";"]
    show (Ne    _ result var var') = concat [t5 result, " := ", t5 var, " /= ", t5 var', ";"]
    show (Gt    _ result var var') = concat [t5 result, " := ", t5 var, " > ",  t5 var', ";"]
    show (Lt    _ result var var') = concat [t5 result, " := ", t5 var, " < ",  t5 var', ";"]
    show (App   _ result var vars) = concat [t5 result, " := ", t5 var, "(", L.intercalate ", " (map t5 vars), ");"]
    show (Call  _ result label)    = result ++ " := call " ++ label
    show (KNormal.True  _ result)  = t5 result ++ " := true;"
    show (KNormal.False _ result)  = t5 result ++ " := false;"
    show (Num   _ result n)        = result ++ " := " ++ show n ++ ";"
    show (Label _ result label)    = result ++ " := " ++ label ++ ";"
    show (If    _ result bool norm norm') = 
        t5 result ++ " := if {\n" ++ unlines (map (\x->tab++show x) bool) 
        ++ "} then {\n" 
        ++ unlines (map (\x->tab++show x) norm)
        ++ "} else {\n"
        ++ unlines (map (\x->tab++show x) norm')
        ++ tab ++ "\n}"
    show (Let    _ result defs norms) = result ++ " := let\n"
        ++ unlines (map (unlines . map (\x->tab++tab++x) . lines . show) defs)
        ++ tab  ++ "in {\n" 
        ++ unlines (map (unlines . map (\x->tab++tab++x) . lines . show) norms) 
        ++ tab ++ "}\n"
 
t5 :: String -> String
t5 = take 7

data NameTag a = Tag String a

returnTag :: a -> NameTag a
returnTag = Tag "_return"

knormalize :: [P.Expr] -> IO [KBlock]
knormalize = mapM (\expr -> case expr of
    P.Define p n as b -> KBlock p n as <$> knormalizeOfTerm b
    P.TypeDef{}       -> error "ERROR: KNormal.hs, knormalize")
    . filter isDefine
    where
    isDefine :: P.Expr -> Bool
    isDefine P.Define{} = Prelude.True
    isDefine _          = Prelude.False

knormalizeOfTerm :: P.Term -> IO [KNormal]
knormalizeOfTerm term = L.reverse 
    <$> (execWriterT . knormWriter . returnTag) term

knormWriter :: NameTag P.Term -> WriterT [KNormal] IO ()
knormWriter (Tag n term) = do
    uuid  <- lift U.genUUID
    uuid' <- lift U.genUUID
    case term of
        P.Add   p t t'   -> tell [Add p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.Sub   p t t'   -> tell [Sub p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.Mul   p t t'   -> tell [Mul p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.Div   p t t'   -> tell [Div p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.Eq    p t t'   -> tell [Eq  p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.Ne    p t t'   -> tell [Ne  p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.Gt    p t t'   -> tell [Gt  p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.Lt    p t t'   -> tell [Lt  p n uuid uuid'] >> knormWriter (Tag uuid t) >> knormWriter (Tag uuid' t') 
        P.True  p        -> tell [KNormal.True p n]
        P.False p        -> tell [KNormal.False p n]
        P.Num   p num    -> tell [Num p n num]
        P.Label p str    -> tell [Label p n str]
        P.Let   p defs norms -> do
            defk <- lift $ knormalize defs
            knorms <- lift $ knormalizeOfTerm norms
            tell [Let p n defk knorms]
        P.If    p b t t' -> do
            bool   <- lift $ knormalizeOfTerm b
            knorm  <- lift $ knormalizeOfTerm t
            knorm' <- lift $ knormalizeOfTerm t'
            tell [If p n bool knorm knorm']
        P.App  p label ts  -> do 
            uuids <- lift $ mapM (const U.genUUID) [1..length ts]
            tell [App p n label uuids] >> mapM_ (knormWriter . Tag uuid) ts
        -- T.Call p t     -> tell [Call p n t]
