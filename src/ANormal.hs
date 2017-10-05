{- |
Module      : ANormal
Description : 木構造の式を展開する
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

とりあえず動かしたいので正しいA正規化を施していない、Boolを数値に展開するのもついでにやっている
-}

module ANormal 
    ( ABlock(..)
    , ANormal(..)
    , anormalize
    ) where

import qualified Parser as S
import qualified KNormal as K
import qualified Util as U

import qualified Data.List as L
import qualified Control.Monad.Writer as W
import Control.Monad (forM_)
import Data.Maybe (isJust)

data ABlock = ABlock 
    { pos  :: S.SourcePos
    , name :: K.Var
    , body :: [ANormal]} 
 
data CroppedBlock =
      ElseBlock
    { pos'  :: S.SourcePos
    , name' :: K.Var
    , croppedVars :: [K.Var]
    , body' :: [K.KNormal]}
    | CBlock
    { pos'  :: S.SourcePos
    , name' :: K.Var
    , body' :: [K.KNormal]
    } deriving Eq

data CallElse = CallElse { addrVar :: K.Var
                         , argVars :: [K.Var]
                         } deriving (Show, Eq)

data ANormal =
      Lambda S.SourcePos K.Var [ANormal]
    | Bne    S.SourcePos K.Var [ANormal] [ANormal] CallElse
    | Add    S.SourcePos K.Var K.Var K.Var
    | Sub    S.SourcePos K.Var K.Var K.Var 
    | Mul    S.SourcePos K.Var K.Var K.Var
    | Div    S.SourcePos K.Var K.Var K.Var
    | Eq     S.SourcePos K.Var K.Var K.Var
    | Ne     S.SourcePos K.Var K.Var K.Var
    | Gt     S.SourcePos K.Var K.Var K.Var
    | Lt     S.SourcePos K.Var K.Var K.Var
    | Call   S.SourcePos K.Var [K.Var]
    | Num    S.SourcePos K.Var Int
    | Label  S.SourcePos K.Var String
    deriving Eq

instance Show ABlock where
    show (ABlock _ n b) = show n ++ " {\n" ++ addIndents b ++ "}\n"

instance Show ANormal where
    show (Add    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " + ",  cut $ show var', ";"]
    show (Sub    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " - ",  cut $ show var', ";"]
    show (Mul    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " * ",  cut $ show var', ";"]
    show (Div    _ result var var') = concat [cut $ show result, " := ", cut $ show var, " / ",  cut $ show var', ";"]
    show (Eq     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " = ",  cut $ show var', ";"]
    show (Ne     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " /= ", cut $ show var', ";"]
    show (Gt     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " > ",  cut $ show var', ";"]
    show (Lt     _ result var var') = concat [cut $ show result, " := ", cut $ show var, " < ",  cut $ show var', ";"]
    show (Call   _ label args)      = "_return := " ++ cut (show label) ++ " (" ++ unwords (map (cut . show) args) ++ ");"
    show (Num    _ result n)        = show result ++ " := " ++ show n ++ ";"
    show (Label  _ result label)    = show result ++ " := " ++ show label ++ ";"
    show (Lambda _ result t)        = 
        "(\\" ++ show result ++ "->\n"
        ++ addIndents t
        ++ "\n)"
    show (Bne     _ result boolNorm norm falseAddr) = 
        show result ++ " := "
        ++ "bne (\n"
        ++ addIndents boolNorm
        ++ ") {\n" 
        ++ addIndents norm
        ++ "} else " ++ show falseAddr

cut :: String -> String
cut = take 7

addIndents :: [ANormal] -> String
addIndents = addIndent . unlines . map show

addIndent :: String -> String
addIndent = unlines . map (\line->tab++line) . lines

tab :: String
tab = L.replicate 4 ' '

anormalize :: [K.KBlock] -> IO [ABlock]
anormalize kblocks = do
    (evaluated, unevaluated) <- W.runWriterT (mapM anormalize' kblocks)
    if null unevaluated then return evaluated
                        else (evaluated ++) <$> anormalizeCropped unevaluated
    where
    anormalize' :: K.KBlock -> W.WriterT [CroppedBlock] IO ABlock
    anormalize' (K.KBlock p n b) = ABlock p n . concat <$> mapM anormalNorm b

anormalizeCropped :: [CroppedBlock] -> IO [ABlock]
anormalizeCropped cblocks = do
    (evaluated, unevaluated) <- W.runWriterT (mapM anormalizeC' cblocks)
    if null unevaluated then return evaluated
                        else (evaluated ++) <$> anormalizeCropped unevaluated
    where
    anormalizeC' :: CroppedBlock -> W.WriterT [CroppedBlock] IO ABlock
    anormalizeC' (CBlock p n b)         = ABlock p n . concat <$> mapM anormalNorm b
    anormalizeC' (ElseBlock p n vars b) = let
        lambdas = map (K.Lambda p) vars :: [[K.KNormal] -> K.KNormal]
        b' = foldr (\x y-> [x y]) b lambdas
        in ABlock p n . concat <$> mapM anormalNorm b'

anormalNorm :: K.KNormal -> W.WriterT [CroppedBlock] IO [ANormal]
anormalNorm knorm = case knorm of
    K.Add    p result var var'            -> return [Add  p result var var']
    K.Sub    p result var var'            -> return [Sub  p result var var']
    K.Mul    p result var var'            -> return [Mul  p result var var']
    K.Div    p result var var'            -> return [Div  p result var var']
    K.Eq     p result var var'            -> return [Eq   p result var var']
    K.Ne     p result var var'            -> return [Ne   p result var var']
    K.Gt     p result var var'            -> return [Gt   p result var var']
    K.Lt     p result var var'            -> return [Lt   p result var var']
    K.Call   p result vars                -> return [Call p result vars]
    K.True   p result                     -> return [Num  p result 1]
    K.False  p result                     -> return [Num  p result 0]
    K.Num    p result n                   -> return [Num   p result n]
    K.Label  p result l                   -> return [Label p result l]
    K.Lambda p result norms               -> do
        anorms <- concat <$> mapM anormalNorm norms
        return [Lambda p result anorms]
    K.Let    _ _ blocks norms             -> do
        forM_ blocks (\block -> case block of
            K.KBlock p n b -> W.tell [CBlock p n b])
        concat <$> mapM anormalNorm norms
    K.If     p result bnorms tnorms fnorm -> do
        uuid <- K.Var <$> W.lift U.genUUID
        abnorms <- concat <$> mapM anormalNorm bnorms
        atnorms <- concat <$> mapM anormalNorm tnorms
        let usingVars = inspectUsingLabels fnorm 
            callElse = CallElse uuid usingVars
        W.tell [ElseBlock p uuid usingVars fnorm] -- こいつは関数になるので、必要な変数を引くようにする
        return [Bne p result abnorms atnorms callElse]

inspectUsingLabels :: [K.KNormal] -> [K.Var]
inspectUsingLabels knorms = filter isGlobalVar
                          $ L.nub
                          $ W.execWriter (mapM inspectUsingLabels' knorms)
    where
    isGlobalVar :: K.Var -> Bool
    isGlobalVar (K.Var n) = isJust $ L.find (== '#') n
    inspectUsingLabels' :: K.KNormal -> W.Writer [K.Var] ()
    inspectUsingLabels' x = case x of
        K.Add{}   -> return ()
        K.Sub{}   -> return ()
        K.Mul{}   -> return ()
        K.Div{}   -> return ()
        K.Eq {}   -> return ()
        K.Ne {}   -> return ()
        K.Gt {}   -> return ()
        K.Lt {}   -> return ()
        K.True{}  -> return ()
        K.False{} -> return ()
        K.Num{}   -> return ()
        K.Call{}  -> return ()
        K.Label  _ _ l           -> if head l == '_' then return () else W.tell [K.Var l]
        K.Lambda _ _ norms       -> mapM_ inspectUsingLabels' norms
        K.If     _ _ bns tns fns -> mapM_ inspectUsingLabels' bns 
                                 >> mapM_ inspectUsingLabels' tns
                                 >> mapM_ inspectUsingLabels' fns
        K.Let    _ _ expr term   -> do
            mapM_ inspectUsingLabels' term
            forM_ expr (\block -> case block of
                K.KBlock _ _ b -> mapM_ inspectUsingLabels' b)
    
