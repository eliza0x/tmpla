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
import qualified Util as U

import qualified Data.Map.Strict as M
-- import Data.Void (Void)

import qualified Control.Eff as E
import Control.Eff ((:>))
import qualified Control.Eff.Exception as EE
import qualified Control.Eff.State.Lazy as ES
import qualified Control.Eff.Lift as EL
import qualified Data.Typeable as T

import qualified Control.Monad as C
import Control.Monad ((<=<))
import qualified Data.List as L
-- import Data.Functor (Functor)
import Data.Maybe (fromMaybe, isJust, isNothing, fromJust)

-- | エラー型
data TypeError = 
      NotMatch{ ans  :: BaseType, actual :: BaseType } -- ^ 目当てのものと違う型が来ている
    | DiffTypes{ type1:: BaseType, type2  :: BaseType } -- ^ 異なった型のものを比較している
    | CanNotApply { type1 :: BaseType, type2 :: BaseType } -- ^ 不可能な型を適用しようとしている
    | NotFound { key :: String } -- ^ 存在しない何かを呼んでいる
    deriving (Show, Eq, T.Typeable)

-- | 環境、二層にすることで参照を表現
data Env = Env { id2typeval   :: M.Map String [String] 
               , typeval2type :: M.Map String BaseType
               } deriving (Show, Eq)

type TypeEnv = M.Map String [BaseType]

-- | 基本型
data BaseType = Int
              | Bool
              | Unk
              | Arr BaseType BaseType
    deriving (Show, Eq)

lookup :: (E.Member (EE.Exc TypeError) r)
       => String
       -> M.Map String v
       -> E.Eff r v
lookup key map = do
    let val = M.lookup key map
    C.when (isNothing val) $ EE.throwExc (NotFound key)
    return $ fromJust val

get :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r)
    => String -- ^ uuid
    -> E.Eff r BaseType
get = getType . head <=< getMiddleId

getMiddleId :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r)
           => String -- ^ uuid
           -> E.Eff r [String]
getMiddleId id = Type.lookup id . id2typeval =<< ES.get

getType :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r)
           => String -- uuid
           -> E.Eff r BaseType
getType middleId = Type.lookup middleId . typeval2type =<< ES.get

update :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r)
       => String -- uuid
       -> [BaseType]
       -> E.Eff r ()
update id types = do
    targetMiddleIds <- getMiddleId id
    env <- ES.get
    let typeval2typeEnv = typeval2type env
    let typeval2type' = foldr (\(k, v) env->M.insert k v env) typeval2typeEnv $ zip targetMiddleIds types
    ES.put $ env { typeval2type = typeval2type' }

updateType :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r)
           => String -- middle uuid
           -> BaseType
           -> E.Eff r ()
updateType id ty = do
    env <- ES.get
    let prevTy = M.lookup id . typeval2type $ env :: Maybe BaseType

    -- 矛盾が生じると死亡
    C.when (isJust prevTy) . C.when (fromJust prevTy /= ty) . EE.throwExc $ NotMatch (fromJust prevTy) ty

    let typeval2type' = M.insert id ty . typeval2type $ env
    ES.put $ env { typeval2type = typeval2type' }

inspectGlobEnv :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r)
               => [N.Expr]
               -> E.Eff r ()
inspectGlobEnv exprs =
    C.forM_ exprs $ \(N.Define pos name body) -> do
        id2typeval'   <- M.insert name [name] . id2typeval  <$> ES.get
        typeval2type' <- M.insert name Unk    . typeval2type <$> ES.get
        ES.put $ Env id2typeval' typeval2type'

typeCheck :: (E.Member (EE.Exc TypeError) r, E.Member (ES.State Env) r, E.SetMember EL.Lift (EL.Lift IO) r)
          => [N.Expr]
          -> E.Eff r TypeEnv
typeCheck exprs = do
    inspectGlobEnv exprs
    undefined

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

