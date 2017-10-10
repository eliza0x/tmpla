{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : KNormal
Description : 木構造の式を展開する
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

木構造の式を展開する、第一段階の中間表現
-}

module KNormal 
    ( KNormal(..)
    , KBlock(..)
    , knormalize
    )where

-- import qualified Data.Map.Strict as M
-- import Data.Maybe
import qualified Data.List as L

import qualified Parser as S
import qualified SyntaxSugar as P
import qualified Util as U
import Type(Var(..))

import qualified Control.Eff as E
import qualified Control.Eff.Writer.Strict as EW
import qualified Control.Eff.Lift as EL
import Control.Eff ((:>))
import Data.Void (Void)

data KBlock = KBlock 
    { pos'  :: S.SourcePos
    , name :: Var
    , body :: [KNormal]
    } deriving Eq

data KNormal =
      Lambda { pos :: S.SourcePos, result :: Var, norms :: [KNormal] }
    | Let    { pos :: S.SourcePos, result :: Var, blocks :: [KBlock], norms :: [KNormal] }
    | If     { pos :: S.SourcePos, result :: Var, cond :: [KNormal], norm1 :: [KNormal], norm2 :: [KNormal] }
    | Add    { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var }
    | Sub    { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var } 
    | Mul    { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var }
    | Div    { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var }
    | Eq     { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var }
    | Ne     { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var }
    | Gt     { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var }
    | Lt     { pos :: S.SourcePos, result :: Var, var1 :: Var, var2 :: Var }
    | Call   { pos :: S.SourcePos, var :: Var, vars :: [Var] }
    | True   { pos :: S.SourcePos, result :: Var }
    | False  { pos :: S.SourcePos, result :: Var }
    | Num    { pos :: S.SourcePos, result :: Var, num :: Int }
    | Label  { pos :: S.SourcePos, result :: Var, label :: String }
    deriving Eq

instance Show KBlock where
    show (KBlock _ n b) = show n ++ " {\n" ++ addIndents b ++ "}\n"

instance Show KNormal where
    show Add{..} = concat [cut $ show result, " := ", cut $ show var1, " + ",  cut $ show var2, ";"]
    show Sub{..} = concat [cut $ show result, " := ", cut $ show var1, " - ",  cut $ show var2, ";"]
    show Mul{..} = concat [cut $ show result, " := ", cut $ show var1, " * ",  cut $ show var2, ";"]
    show Div{..} = concat [cut $ show result, " := ", cut $ show var1, " / ",  cut $ show var2, ";"]
    show Eq {..} = concat [cut $ show result, " := ", cut $ show var1, " = ",  cut $ show var2, ";"]
    show Ne {..} = concat [cut $ show result, " := ", cut $ show var1, " /= ", cut $ show var2, ";"]
    show Gt {..} = concat [cut $ show result, " := ", cut $ show var1, " > ",  cut $ show var2, ";"]
    show Lt {..} = concat [cut $ show result, " := ", cut $ show var1, " < ",  cut $ show var2, ";"]
    show Call{..} = "_return := " ++ cut (show var) ++ " (" ++ unwords (map (cut . show) vars) ++ ");"
    show KNormal.True {..} = show result ++ " := true;"
    show KNormal.False{..} = show result ++ " := false;"
    show Num{..}   = show result ++ " := " ++ show num ++ ";"
    show Label{..} = show result ++ " := " ++ show label ++ ";"
    show Lambda{..} = 
        "(\\" ++ show result ++ "->\n"
        ++ addIndents norms
        ++ "\n)"
    show If{..} =
        show result ++ " := "
        ++ "if (\n"
        ++ addIndents cond
        ++") {\n" 
        ++ addIndents norm1
        ++ "} else {\n" 
        ++ addIndents norm2
        ++ "}"
    show Let{..} = 
        show result ++ " := let {\n"
        ++ (concatMap addIndent . lines . init . concat $ map show blocks)
        ++ "} in {\n" 
        ++ addIndents norms
        ++ "}\n"

cut :: String -> String
cut = take 7

addIndents :: [KNormal] -> String
addIndents = addIndent . unlines . map show

addIndent :: String -> String
addIndent = unlines . map (\line->tab++line) . lines
    where
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
knormalTag (Tag n prenormalizedterm) = do
    uuid  <- Var <$> EL.lift U.genUUID
    uuid' <- Var <$> EL.lift U.genUUID
    case prenormalizedterm of
        P.Add{..} -> arith (Add pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.Sub{..} -> arith (Sub pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.Mul{..} -> arith (Mul pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.Div{..} -> arith (Div pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.Eq{..}  -> arith (Eq  pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.Ne{..}  -> arith (Ne  pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.Gt{..}  -> arith (Gt  pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.Lt{..}  -> arith (Lt  pos n uuid uuid') (Tag uuid term1) (Tag uuid term2)
        P.True {..} -> EW.tell [KNormal.True pos n]
        P.False{..} -> EW.tell [KNormal.False pos n]
        P.Num  {..} -> EW.tell [Num pos n num]
        P.Label{..} -> EW.tell [Label pos n label]
        P.Let  {..} -> do
            blocks <- EL.lift $ knormalize exprs
            knorms <- EL.lift $ knormalKNormal (returnTag term)
            EW.tell [Let pos n blocks knorms]
        P.Lambda{..} -> do
            knorms <- EL.lift  $ knormalKNormal (Tag n term)
            EW.tell [Lambda pos (Var label) knorms]
        P.If{..} -> do
            boolNorm  <- EL.lift $ knormalKNormal (returnTag cond)
            trueNorm  <- EL.lift $ knormalKNormal (returnTag term1)
            falseNorm <- EL.lift $ knormalKNormal (returnTag term2)
            EW.tell [If  pos n boolNorm trueNorm falseNorm]
        P.App{..} -> do
            let expandedApp = expandApp term2
            uuids <- EL.lift $ map Var <$> mapM (const U.genUUID) [1..length expandedApp]
            EW.tell [Call pos uuid uuids]
            knormalTag (Tag uuid term1)
            mapM_ knormalTag (zipWith Tag uuids expandedApp)
    where
    arith :: ( E.Member (EW.Writer [KNormal]) r
             , E.SetMember EL.Lift (EL.Lift IO) r)
             => KNormal
             -> NameTag P.Term 
             -> NameTag P.Term
             -> E.Eff r ()
    arith knorm t t' =  EW.tell [knorm]
                    >> knormalTag t 
                    >> knormalTag t'
    expandApp :: P.Term -> [P.Term]
    expandApp (P.App _ t t') = t : expandApp t'
    expandApp x              = [x]

