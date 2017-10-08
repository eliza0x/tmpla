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
    , CallElse(..)
    , ANormal(..)
    , anormalize
    ) where

import qualified Parser as S
import qualified KNormal as K
import qualified Util as U
import Type (Var(..))

import qualified Data.List as L
import qualified Control.Monad.Writer as W
import qualified Control.Monad.State.Strict as S
import Control.Monad (forM_)
import Data.Maybe (isJust)

data ABlock = ABlock 
    { pos  :: S.SourcePos
    , name :: Var
    , body :: [ANormal]} 
 
data CroppedBlock =
      ElseBlock 
        S.SourcePos 
        Var       -- ^ else用のブロック名
        Var       -- ^ continue用のブロック名
        [Var]     -- ^ 内部で使用している変数名
        [K.KNormal]
    | CBlock S.SourcePos Var [K.KNormal]
    deriving Eq

data CallElse = CallElse 
                    Var   -- ^ Else label用変数
                    Var   -- ^ Continue label変数
                    [Var] -- ^ 内部で使用している変数
    deriving (Show, Eq)

data ANormal =
      Lambda S.SourcePos Var [ANormal]
    | Bne    S.SourcePos Var [ANormal] [ANormal] CallElse
    | Add    S.SourcePos Var Var Var
    | Sub    S.SourcePos Var Var Var 
    | Mul    S.SourcePos Var Var Var
    | Div    S.SourcePos Var Var Var
    | Eq     S.SourcePos Var Var Var
    | Ne     S.SourcePos Var Var Var
    | Gt     S.SourcePos Var Var Var
    | Lt     S.SourcePos Var Var Var
    | Call   S.SourcePos Var [Var]
    | Num    S.SourcePos Var Int
    | Label  S.SourcePos Var String
    | Jot    S.SourcePos Var
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
    show (Jot    _ label)           = "jot "++show label++";"
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
    anormalizeC' (ElseBlock p n c vars b) = let
        lambdas =  map (K.Lambda p) vars :: [[K.KNormal] -> K.KNormal]
        b' = foldr (\x y-> [x y]) b lambdas
        in ABlock p n . (++[Jot p c]) . map (addPrefix "else#")  . concat <$> mapM anormalNorm b'

addPrefix :: String -> ANormal -> ANormal
addPrefix prefix knorm = S.evalState (addPrefix' prefix knorm) []

addPrefix' :: String -> ANormal -> S.State [Var] ANormal
addPrefix' prefix knorm = case knorm of
    Add    p result a b    -> return $ Add  p result a b    
    Sub    p result a b    -> return $ Sub  p result a b    
    Mul    p result a b    -> return $ Mul  p result a b    
    Div    p result a b    -> return $ Div  p result a b    
    Eq     p result a b    -> return $ Eq   p result a b    
    Ne     p result a b    -> return $ Ne   p result a b    
    Gt     p result a b    -> return $ Gt   p result a b    
    Lt     p result a b    -> return $ Lt   p result a b    
    Call   p result args   -> return $ Call p result args   
    Num    p result n      -> return $ Num  p result n      
    Label  p result l      -> do
        list <- S.get
        let l' = if isJust (L.find (==Var l) list) then prefix++l else l
        return $ Label  p result l'
    Jot    p a             -> return $ Jot p a
    Bne    p result banorms tanorms celse -> 
        Bne p result <$> mapM (addPrefix' prefix) banorms
                     <*> mapM (addPrefix' prefix) tanorms
                     <*> return celse
    Lambda p arg anorms -> do
        S.modify (arg:)
        Lambda p (Var $ prefix ++ fromVar arg) <$> mapM (addPrefix' prefix) anorms
 
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
        elseAddr     <- Var <$> W.lift U.genUUID
        continueAddr <- Var <$> W.lift U.genUUID
        abnorms <- concat <$> mapM anormalNorm bnorms
        atnorms <- concat <$> mapM anormalNorm tnorms
        let usingVars = inspectUsingLabels fnorm 
            callElse = CallElse elseAddr continueAddr usingVars
        W.tell [ElseBlock p elseAddr continueAddr usingVars fnorm] -- こいつは関数になるので、必要な変数を引くようにする
        return [Bne p result abnorms atnorms callElse]

inspectUsingLabels :: [K.KNormal] -> [Var]
inspectUsingLabels knorms = filter isGlobalVar
                          $ L.nub
                          $ W.execWriter (mapM inspectUsingLabels' knorms)
    where
    isGlobalVar :: Var -> Bool
    isGlobalVar (Var n) = isJust $ L.find (== '#') n
    inspectUsingLabels' :: K.KNormal -> W.Writer [Var] ()
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
        K.Label  _ _ l           -> if head l == '_' then return () else W.tell [Var l]
        K.Lambda _ _ norms       -> mapM_ inspectUsingLabels' norms
        K.If     _ _ bns tns fns -> mapM_ inspectUsingLabels' bns 
                                 >> mapM_ inspectUsingLabels' tns
                                 >> mapM_ inspectUsingLabels' fns
        K.Let    _ _ expr term   -> do
            mapM_ inspectUsingLabels' term
            forM_ expr (\block -> case block of
                K.KBlock _ _ b -> mapM_ inspectUsingLabels' b)
    
