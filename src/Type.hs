{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-#LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : Type
Description : 型検査をする
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

静的型検査をする。型推論は出来ないので明示的に型を書く必要がある。
-}

module Type
    ( typeCheck
    , TypeError(..)
    )where
 
import qualified Parser as P
import qualified PNormal as N

import qualified Data.Map.Strict as M
import Data.Void (Void)

import qualified Control.Eff as E
import Control.Eff ((:>))
import qualified Control.Eff.Exception as EE
import qualified Control.Eff.State.Lazy as ES
import qualified Data.Typeable as T

-- import qualified Control.Monad as C
import qualified Data.List as L
-- import Data.Maybe (fromMaybe)

type Env     = M.Map String Type
type LocalEnv = M.Map  String Local

data Local = Local  { funcName :: String, applyTime :: Int, typeOf :: Type }

-- | エラー型
data TypeError = NotMatch{  pos :: P.SourcePos, ans  :: Type, actual :: Type } -- ^ 目当てのものと違う型が来ている
               | DiffTypes{ pos :: P.SourcePos, type1:: Type, type2  :: Type } -- ^ 異なった型のものを比較している
               deriving (Show, Eq, T.Typeable)

-- TODO: #2 本当はTermから一意に定まるようになっているほうがいい気がする
data Type = Int     P.SourcePos
          | Bool    P.SourcePos
          | Unknown P.SourcePos String
          | Arr     P.SourcePos [Type]

instance Show Type where
    show (Int _)       = "Int"
    show (Bool _)      = "Bool"
    show (Unknown _ s) = s
    show (Arr _ t)   = L.intercalate " -> " $ map show t

instance Eq Type where
    Int{}     == Int{}  = True
    Bool{}    == Bool{} = True
    Unknown{} == _      = True
    Arr _ t == Arr _ t' = and $ zipWith (==) t t'
    Int{}     == _      = False
    Bool{}    == _      = False
    Arr{}     == _      = False

typeCheck :: [N.Expr]
          -> E.Eff (EE.Exc TypeError :> Void) Bool
typeCheck exprs = let
    env      = inspectGlobEnv exprs                   :: Env
    localEnv = M.mapWithKey (\k v -> Local k 0 v) env :: LocalEnv
    isDefine N.Define{}  = True
    isDefine N.TypeDef{} = False
    in and <$> ES.evalState env 
    (mapM (compareType localEnv) $ filter isDefine exprs :: E.Eff (ES.State Env :> EE.Exc TypeError :> Void) [Bool])

compareType :: (E.Member (ES.State Env) r, E.Member (EE.Exc TypeError) r)
               => LocalEnv
               -> N.Expr 
               -> E.Eff r Bool
compareType localEnv (N.Define p n b) = do
    env <- M.lookup n ES.get
    return undefined

evalExpr :: ( E.Member (ES.State LocalEnv) r -- ^ ローカル変数を管理する
            , E.Member (ES.State Env) r      -- ^ グローバル変数を管理する、不定項は処理に応じて書き換えられていく
            , E.Member (EE.Exc TypeError) r)
         => N.Expr
         -> E.Eff r Type
evalExpr = undefined


{-
eval :: (EE.MonadThrow m) => Env -> N.Term -> m Type
eval env term = case term of
    N.Add p t t'  -> arith p t t'
    N.Sub p t t'  -> arith p t t'
    N.Mul p t t'  -> arith p t t'
    N.Div p t t'  -> arith p t t'
    N.Eq  p t t'  -> eq p t t'
    N.Ne  p t t'  -> eq p t t'
    N.Gt  p t t'  -> eq p t t'
    N.Lt  p t t'  -> eq p t t'
    N.True  p     -> return $ Bool p
    N.False p     -> return $ Bool p
    N.Num p _     -> return $ Int p
    N.Label _ _   -> undefined -- TODO
    N.If p b t t' -> undefined -- TODO
    N.Let p es t  -> undefined -- TODO
    N.App p t t'  -> undefined -- TODO
    where 
    eq :: (EE.MonadThrow m) => P.SourcePos -> N.Term -> N.Term -> m Type
    eq p t t' = do
        ty  <- eval env t
        ty' <- eval env t'
        if ty == ty'
            then return $ Bool p
            else EE.throwM $ DiffTypes p ty ty'

    arith :: (EE.MonadThrow m) => P.SourcePos -> N.Term -> N.Term -> m Type
    arith p t t' = do
        ty  <- eval env t
        ty' <- eval env t'
        case (ty == Int p, ty' == Int p) of
            (True, True)   -> return $ Int p
            (False, _)  -> EE.throwM . NotMatch p ty' $ Int p
            (_, False)  -> EE.throwM . NotMatch p ty  $ Int p

    isInt :: (EE.MonadThrow m) => P.SourcePos -> N.Term -> m Bool
    isInt p t'' = (== Int p) <$> eval env t''
-}

inspectGlobEnv :: [N.Expr] -> Env
inspectGlobEnv = undefined

