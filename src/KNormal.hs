{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : KNormal
Description : 木構造の式を展開する
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

木構造の式を展開する、第一段階の中間表現
-}

module KNormal where

-- import qualified Data.Map.Strict as M
-- import Data.Maybe
import qualified Data.List as L

import qualified Parser as S
import qualified PNormal as P
import qualified Util as U
import Type(Var(..))

import qualified Control.Eff as E
import qualified Control.Eff.Writer.Strict as EW
import qualified Control.Eff.Lift as EL
import Control.Eff ((:>))
import Data.Void (Void)

data KBlock = KBlock 
    { pos  :: S.SourcePos
    , name :: Var
    , body :: [KNormal]
    } deriving Eq

data KNormal =
      Lambda S.SourcePos Var [KNormal]
    | Let    S.SourcePos Var [KBlock] [KNormal]
    | If     S.SourcePos Var [KNormal] [KNormal] [KNormal]
    | Add    S.SourcePos Var Var Var
    | Sub    S.SourcePos Var Var Var 
    | Mul    S.SourcePos Var Var Var
    | Div    S.SourcePos Var Var Var
    | Eq     S.SourcePos Var Var Var
    | Ne     S.SourcePos Var Var Var
    | Gt     S.SourcePos Var Var Var
    | Lt     S.SourcePos Var Var Var
    | Call   S.SourcePos Var [Var]
    | True   S.SourcePos Var
    | False  S.SourcePos Var
    | Num    S.SourcePos Var Int
    | Label  S.SourcePos Var String
    deriving Eq

instance Show KBlock where
    show (KBlock _ n b) = show n ++ " {\n" ++ addIndents b ++ "}\n"

instance Show KNormal where
    show (Add    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " + ",  cut $ show var', ";"]
    show (Sub    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " - ",  cut $ show var', ";"]
    show (Mul    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " * ",  cut $ show var', ";"]
    show (Div    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " / ",  cut $ show var', ";"]
    show (Eq     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " = ",  cut $ show var', ";"]
    show (Ne     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " /= ", cut $ show var', ";"]
    show (Gt     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " > ",  cut $ show var', ";"]
    show (Lt     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " < ",  cut $ show var', ";"]
    show (Call   _ label args)      = "_return := " ++ cut (show label) ++ " (" ++ unwords (map (cut . show) args) ++ ");"
    show (KNormal.True  _ result)   = show result ++ " := true;"
    show (KNormal.False _ result)   = show result ++ " := false;"
    show (Num    _ result n)        = show result ++ " := " ++ show n ++ ";"
    show (Label  _ result label)    = show result ++ " := " ++ show label ++ ";"
    show (Lambda _ result t)        = 
        "(\\" ++ show result ++ "->\n"
        ++ addIndents t
        ++ "\n)"
    show (If     _ result boolNorm norm norm') = 
        show result ++ " := "
        ++ "if (\n"
        ++ addIndents boolNorm
        ++") {\n" 
        ++ addIndents norm
        ++ "} else {\n" 
        ++ addIndents norm'
        ++ "}"
    show (Let    _ result defs norms) = 
        show result ++ " := let {\n"
        ++ (concatMap addIndent . lines . init . concat $ map show defs)
        ++ "} in {\n" 
        ++ addIndents norms
        ++ "}\n"

cut :: String -> String
cut = take 7

addIndents :: [KNormal] -> String
addIndents = addIndent . unlines . map show

addIndent :: String -> String
addIndent = unlines . map (\line->tab++line) . lines

tab :: String
tab = L.replicate 4 ' '

data NameTag a = Tag Var a

returnTag :: a -> NameTag a
returnTag = Tag (Var "_return")

knormalize :: [P.Expr] -> IO [KBlock]
knormalize = mapM (\(P.Define p n b) -> KBlock p (Var n) <$> knormalKNormal (returnTag b))

knormalKNormal :: NameTag P.Term -> IO [KNormal]
knormalKNormal tag = do
    (knormsRev, _) <- EL.runLift
        $ EW.runMonoidWriter
          (knormalTag tag :: E.Eff (EW.Writer [KNormal] :> EL.Lift IO :> Void) ())
    return $ L.reverse knormsRev

knormalTag :: ( E.Member (EW.Writer [KNormal]) r    -- 展開リスト
            , E.SetMember EL.Lift (EL.Lift IO) r) -- IO
            => NameTag P.Term 
            -> E.Eff r ()
knormalTag (Tag n term) = do
    uuid  <- Var <$> EL.lift U.genUUID
    uuid' <- Var <$> EL.lift U.genUUID
    case term of
        P.Add    p t t'     -> EW.tell [Add p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.Sub    p t t'     -> EW.tell [Sub p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.Mul    p t t'     -> EW.tell [Mul p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.Div    p t t'     -> EW.tell [Div p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.Eq     p t t'     -> EW.tell [Eq  p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.Ne     p t t'     -> EW.tell [Ne  p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.Gt     p t t'     -> EW.tell [Gt  p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.Lt     p t t'     -> EW.tell [Lt  p n uuid uuid'] >> knormalTag (Tag uuid t) >> knormalTag (Tag uuid' t')
        P.True   p          -> EW.tell [KNormal.True p n]
        P.False  p          -> EW.tell [KNormal.False p n]
        P.Num    p num      -> EW.tell [Num p n num]
        P.Label  p l        -> EW.tell [Label p n l]
        P.Let    p exprs t  -> do
            blocks <- EL.lift $ knormalize exprs
            knorms <- EL.lift $ knormalKNormal (returnTag t)
            EW.tell [Let p n blocks knorms]
        P.Lambda p l _ t    -> do
            knorms <- EL.lift  $ knormalKNormal (Tag n t)
            EW.tell [Lambda p (Var l) knorms]
        P.If     p bt tt ft -> do
            boolNorm  <- EL.lift $ knormalKNormal (returnTag bt)
            trueNorm  <- EL.lift $ knormalKNormal (returnTag tt)
            falseNorm <- EL.lift $ knormalKNormal (returnTag ft)
            EW.tell [If  p n boolNorm trueNorm falseNorm]
        P.App    p t t'     -> do
            let l = foldApp t'
            uuids <- EL.lift $ map Var <$> mapM (const U.genUUID) [1..length l]
            EW.tell [Call p uuid uuids]
            knormalTag (Tag uuid t)
            mapM_ knormalTag (zipWith Tag uuids l)
    where
    foldApp :: P.Term -> [P.Term]
    foldApp (P.App _ t t') = t : foldApp t'
    foldApp x              = [x]

