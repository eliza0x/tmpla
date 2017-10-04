{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Type
Description : 型検査をする
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

静的型検査をする。
-}

module Type (
      TypeCheckError(..)
    , typeCheck
    , Env
    ) where
 
import qualified Parser as P
import qualified PNormal as N
-- import qualified Util as U

import qualified Data.Map.Strict as M
import Data.Maybe

-- extensible effects
import qualified Control.Eff as E
import qualified Control.Eff.Exception as EX
import qualified Control.Eff.State.Lazy as ES
-- import qualified Control.Eff.Lift as EL
-- import Control.Eff ((:>))
-- import Data.Void (Void)
import Data.Typeable (Typeable)

data Type = TInt
          | TBool
          | TUnkL String
          | TUnk
          | Arr Type Type
          deriving (Show, Eq)

-- | エラー型
data TypeCheckError =
      TypeMismatch { actual :: Type, expected :: Type } -- ^ 期待していた型が来なかった場合
    | DiffTypes { type1 :: Type, type2 :: Type }        -- ^ 比較演算子等で異なった型を比較しようとした時
    | CanNotApply { type1 :: Type, type2 :: Type }      -- ^ 適用不可な値に値を適用している
    | NotFound { key :: String }                        -- ^ 存在しない何かを呼んでいる
    deriving (Show, Eq, Typeable)

type Env = M.Map String Type

typeCheck :: (E.Member (EX.Exc TypeCheckError) r)
          => [N.Expr]
          -> E.Eff r Env
typeCheck exprs = evalExprs exprs 50 $ inspectExprs exprs M.empty

-- 環境に関数の型推論の結果を追記している
inspectExprs ::[N.Expr] -> Env -> Env
inspectExprs exprs env = foldr (\(N.Define _ n _) -> M.insert n TUnk) env exprs

-- | 型推論の結果が変化しなくなるまで再帰する
-- 型検査は上から流しているだけなので一度では決定不能な項が存在する
-- 始めに決定できない項を呼び出す項がだれだけ積んであるかで決定回数は決まる

evalExprs :: (E.Member (EX.Exc TypeCheckError) r)
          => [N.Expr] -- ^ 構文木
          -> Int    -- ^ 再帰上限
          -> Env    -- ^ 環境
          -> E.Eff r Env
evalExprs exprs iterNum env = do
    env' <- ES.execState env $ mapM_ evalExpr exprs
    case (env /= env', iterNum  > 1) of
        (True, True)   -> evalExprs exprs (iterNum-1) env'
        (True, False)  -> return env'
        (False, _)     -> return env'
    where
    evalExpr :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
             => N.Expr
             -> E.Eff r ()
    evalExpr (N.Define _ n tm) = do
        evaluated <- eval tm
        ES.modify (M.insert n evaluated)

eval :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
     => N.Term 
     -> E.Eff r Type
eval term = case term of
    N.Add _ t t'        -> arith t t'
    N.Sub _ t t'        -> arith t t'
    N.Mul _ t t'        -> arith t t'
    N.Div _ t t'        -> arith t t'
    N.Eq  _ t t'        -> eqType t t'
    N.Ne  _ t t'        -> eqType t t'
    N.Gt  _ t t'        -> eqType t t'
    N.Lt  _ t t'        -> eqType t t'
    N.True  _           -> return TBool
    N.False _           -> return TBool
    N.Num _ _           -> return TInt
    N.Label _ l         -> label l
    N.App   _ t t'      -> app t t'
    N.If _ b t t'       -> evalIf b t t'
    N.Lambda _ n ty tr  -> lambda n (fromString ty) tr
    N.Let _ exprs tr    -> do
        env' <- evalExprs exprs 50 . inspectExprs exprs =<< ES.get
        ES.put env'
        eval tr
    
fromString :: String -> Type
fromString str = case str of
    "Int"  -> TInt
    "Bool" -> TBool
    _      -> TUnkL str

-- | Int -> Int -> Int の算術演算を処理する
arith :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
      => N.Term 
      -> N.Term
      -> E.Eff r Type
arith t1 t1' = do
    ty <- eval t1
    ty' <- eval t1'
    b  <- isInt ty
    b' <- isInt ty'
    case (b, b') of
        (True,  True)  -> return TInt
        (True,  False) -> EX.throwExc $ TypeMismatch ty' TInt
        (False, True)  -> EX.throwExc $ TypeMismatch ty  TInt
        (False, False) -> EX.throwExc $ TypeMismatch ty  TInt
    where
    isInt :: (E.Member (ES.State Env) r)
          => Type -> E.Eff r Bool
    isInt TInt      = return True
    isInt TUnk      = return True
    isInt (TUnkL l) = do
        ES.modify (M.insert l TInt)
        return True
    isInt _         = return False

-- | a -> a -> Bool の比較演算を処理する
eqType :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
       => N.Term 
       -> N.Term
       -> E.Eff r Type
eqType t t' = do
    b  <- eval t
    b' <- eval t'
    updateEnv b b'
    if b == b'
        then return TBool    
        else error $ "\""++show t++"\" and \""++show t'++"\" are diff types"
    where
    updateEnv :: (E.Member (ES.State Env) r)
              => Type -> Type -> E.Eff r ()
    updateEnv t1 t1' = case (labelFromType t1, labelFromType t1') of
        (Just _, Just _)  -> return ()
        (Just l, Nothing)  -> ES.modify (M.insert l t1')
        (Nothing, Just l') -> ES.modify (M.insert l' t1)
        (Nothing, Nothing) -> return ()
    labelFromType :: Type -> Maybe String
    labelFromType (TUnkL l) = Just l
    labelFromType _         = Nothing

-- | Labelを処理する
label :: (E.Member (ES.State Env) r)
      => String
      -> E.Eff r Type
label l = fromMaybe (TUnkL l) -- 環境に存在しない変数の場合ラベル付き未確定型を返す
        . M.lookup l <$> ES.get

-- | 適用を処理する
app :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
    => N.Term 
    -> N.Term 
    -> E.Eff r Type
app t t' = do
    t1  <- eval t
    t1' <- eval t'
    appType t1 t1'
    
appType :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
        => Type
        -> Type
        -> E.Eff r Type
appType t t' = case t of
    TInt        -> EX.throwExc $ CanNotApply t t'
    TBool       -> EX.throwExc $ CanNotApply t t'
    TUnk        -> return TUnk
    TUnkL l     -> do
        t1 <- fromMaybe (Arr t' (TUnkL l)) . M.lookup l <$> ES.get
        ES.modify (M.insert l t1)
        return $ TUnkL l
    Arr  t1 t1' -> if t1 == t' 
        then return t1' 
        else EX.throwExc $ CanNotApply t t'
-- | 条件分岐を処理
evalIf :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
       => N.Term
       -> N.Term 
       -> N.Term
       -> E.Eff r Type
evalIf b t t' = do
    b1  <- eval b
    t1  <- eval t
    t1' <- eval t'
    case (b1 == TBool, t1 == t1') of
        (True,  True)  -> return t1
        (False, True)  -> EX.throwExc $ TypeMismatch b1 TBool
        (True,  False) -> EX.throwExc $ DiffTypes t1 t1'
        (False, False) -> EX.throwExc $ DiffTypes t1 t1'

-- | ラムダ式を処理
lambda :: (E.Member (ES.State Env) r, E.Member (EX.Exc TypeCheckError) r)
       => String
       -> Type
       -> N.Term
       -> E.Eff r Type
lambda l ty tr = do
    ES.modify (M.insert l ty) -- 引数の型を環境に追加している
    Arr ty <$> eval tr
 
{-
main :: IO ()
main = print . E.run $ EX.runExc (typeCheck source :: E.Eff (EX.Exc TypeCheckError :> Void) Env)
-}

