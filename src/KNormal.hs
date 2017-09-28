{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

type Var = String

type Label = String

data KBlock = KBlock 
    { pos  :: P.SourcePos
    , name :: Label
    , args :: [Label]
    , body :: [KNormal]
    } deriving Eq

data KNormal =
      If    P.SourcePos Var [KNormal] [KNormal] [KNormal]
    | Add   P.SourcePos Var Var Var
    | Sub   P.SourcePos Var Var Var 
    | Mul   P.SourcePos Var Var Var
    | Div   P.SourcePos Var Var Var
    | Eq    P.SourcePos Var Var Var
    | Ne    P.SourcePos Var Var Var
    | Gt    P.SourcePos Var Var Var
    | Lt    P.SourcePos Var Var Var
    | Call  P.SourcePos Var Label [Var]
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
    show (Add   _ result var var') = concat [t5 result, " = ", t5 var, " + ",  t5 var', ";"]
    show (Sub   _ result var var') = concat [t5 result, " = ", t5 var, " - ",  t5 var', ";"]
    show (Mul   _ result var var') = concat [t5 result, " = ", t5 var, " * ",  t5 var', ";"]
    show (Div   _ result var var') = concat [t5 result, " = ", t5 var, " / ",  t5 var', ";"]
    show (Eq    _ result var var') = concat [t5 result, " = ", t5 var, " = ",  t5 var', ";"]
    show (Ne    _ result var var') = concat [t5 result, " = ", t5 var, " /= ", t5 var', ";"]
    show (Gt    _ result var var') = concat [t5 result, " = ", t5 var, " > ",  t5 var', ";"]
    show (Lt    _ result var var') = concat [t5 result, " = ", t5 var, " < ",  t5 var', ";"]
    show (Call  _ result var vars) = concat [t5 result, " = ", t5 var, "(", L.intercalate ", " (map t5 vars), ");"]
    show (KNormal.True  _ result)          = t5 result ++ " = true;"
    show (KNormal.False _ result)          = t5 result ++ " = false;"
    show (Num   _ result n)        = result ++ " = " ++ show n ++ ";"
    show (Label _ result label)    = result ++ " = " ++ label ++ ";"
    show (If    _ result bool norm norm') = 
        t5 result ++ " = if {\n" ++ unlines (map (\x->tab++tab++show x) bool) 
        ++ tab ++ "} then {\n" 
        ++ unlines (map (\x->tab++tab++show x) norm) 
        ++ tab ++ "} else {\n"
        ++ unlines (map (\x->tab++tab++show x) norm')++ tab ++ "}"
 
t5 :: String -> String
t5 = take 7

data NameTag a = Tag String a

returnTag :: a -> NameTag a
returnTag = Tag "_return"

knormalize :: [P.Expr] -> IO [KBlock]
knormalize = mapM (\expr -> case expr of
    P.Define p n as b -> KBlock p n as <$> knormalizeOfTerm b)

knormalizeOfTerm :: P.Term -> IO [KNormal]
knormalizeOfTerm term = L.reverse 
    <$> (execWriterT . knormWriter . returnTag) term

genUUID :: IO String
genUUID = ('_':) . U.toString <$> U.nextRandom

knormWriter :: NameTag P.Term -> WriterT [KNormal] IO ()
knormWriter (Tag n term) = do
    uuid  <- lift genUUID
    uuid' <- lift genUUID
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
        P.If    p b t t' -> do
            bool   <- lift $ knormalizeOfTerm b
            knorm  <- lift $ knormalizeOfTerm t
            knorm' <- lift $ knormalizeOfTerm t'
            tell [If p n bool knorm knorm']
        P.Call  p label ts  -> do
            uuids <- lift $ mapM (const genUUID) [1..length ts]
            tell [Call p n label uuids] >> mapM_ (knormWriter . Tag uuid) ts

