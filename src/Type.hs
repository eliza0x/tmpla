{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Type
Description : 型検査をする
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

静的型検査をする。型推論は出来ないので明示的に型を書く必要がある。
-}

module Type where
 
import qualified Parser as P
import qualified PNormal as N

import qualified Data.Map.Strict as M
import Data.Void (Void)

import qualified Control.Eff as E
import Control.Eff ((:>))
import qualified Control.Eff.Exception as EE
import qualified Control.Eff.State.Lazy as ES
import qualified Data.Typeable as T

import qualified Control.Monad as C
import qualified Data.List as L
-- import Data.Functor (Functor)
-- import Data.Maybe (fromMaybe)

type Env = M.Map String TypeAndRef

-- | 参照と型の直和
data TypeAndRef = TypeAndRef { ref    :: Maybe String
                             , typeOf :: Type
                             } deriving (Show, Eq)

-- | エラー型
data TypeError = 
      NotMatch{ ans  :: Type, actual :: Type } -- ^ 目当てのものと違う型が来ている
    | DiffTypes{ type1:: Type, type2  :: Type } -- ^ 異なった型のものを比較している
    | CanNotApply { type1 :: Type, type2 :: Type } -- ^ 不可能な型を適用しようとしている
    | NotFound { pos :: P.SourcePos, key :: String } -- ^ 存在しない何かを呼んでいる
    deriving (Show, Eq, T.Typeable)

-- TODO: #2 本当はTermから一意に定まるようになっているほうがいい気がする
data Type = Int     P.SourcePos
          | Bool    P.SourcePos
          | Unknown P.SourcePos
          | Arr     P.SourcePos [Type]

instance Show Type where
    show (Int _)       = "Int"
    show (Bool _)      = "Bool"
    show (Unknown _)   = "Unknown"
    show (Arr _ ts)  = L.intercalate " -> " $ map show ts

instance Eq Type where
    Int{}     == Int{}      = True
    Bool{}    == Bool{}     = True
    Unknown{} == _          = True
    Arr _ ts  == Arr _ ts'  = and $ zipWith (==) ts ts'
    Int{}     == _          = False
    Bool{}    == _          = False
    Arr{}     == _          = False
 
-- | 環境から型を持って来る
-- なければ死ぬ
typeFromEnv :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r)
            => P.SourcePos
            -> String
            -> E.Eff r TypeAndRef
typeFromEnv pos key = do
    env <- ES.get
    case M.lookup key env of
        Nothing -> EE.throwExc $ NotFound pos key
        Just t  -> return t

-- | 何に何を適用すると何が帰ってくるか
typeApply :: (E.Member (EE.Exc TypeError) r)
          => Type -- 何に
          -> Type -- 何を
          -> E.Eff r Type
typeApply t t' = do
    case t of
        (Int p)     -> EE.throwExc $ CanNotApply (Int p)  t' 
        (Bool p)    -> EE.throwExc $ CanNotApply (Bool p) t' 
        (Unknown p) -> return $ Unknown p
        (Arr p ts)  -> case (head ts == t', length ts == 2) of
            (True, True)  -> return $ last ts
            (True, False) -> return . Arr p $ tail ts
            (False, _)    -> EE.throwExc $ NotMatch (head ts) t'

typeCheck :: [N.Expr]
          -> E.Eff (EE.Exc TypeError :> Void) Bool
typeCheck exprs = undefined

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

-}
