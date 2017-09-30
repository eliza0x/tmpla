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
    )where
 
import qualified Parser as P
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)

type Env = M.Map String Type

-- TODO: #2 本当はTermから一意に定まるようになっているほうがいい気がする
data Type = Int  P.SourcePos
          | Bool P.SourcePos
          | Func P.SourcePos [Type] 

instance Show Type where
    show (Int _)       = "Int"
    show (Bool _)      = "Bool"
    show (Func _ args) = L.intercalate "->" $ map show args

instance Eq Type where
    Int{}  == Int{}  = True
    Int{}  == Bool{} = False
    Int{}  == Func{} = False
    Bool{} == Int{}  = False
    Bool{} == Bool{} = True
    Bool{} == Func{} = False
    Func{} == Int{}  = False
    Func{} == Bool{} = False
    (Func _ x) == (Func _ y) = x == y

typeCheck :: [P.Expr] -> Bool
typeCheck exprs = all (typeCheck' $ globalEnv exprs)  -- 全ての型検査の結果で積を取っている
                . filter (not . isTypeDef) $ exprs -- TypeDefを排除する

-- 演繹と環境が一致するか確認する
typeCheck' :: Env -> P.Expr -> Bool
typeCheck' _ (P.TypeDef p _ _) = error $ "[ERROR] Type.hs: typeCheck\n" ++ P.sourcePosPretty p
typeCheck' env (P.Define p l as t)  = let
    env' = foldr (\(k,v) e->M.insert k v e) env $ zip as $ case typeFromEnv env p l of
        (Func _ asType) -> asType
        x               -> [x]
    -- TODO: #1 何故かうまいこといかないので応急処置的にshowを噛ませている、のちのち治すこと
    in show (eval env' t) == show ((retType . typeFromEnv env' p) l)

    where
    -- 環境から型を求める
    typeFromEnv :: Env -> P.SourcePos -> String -> Type
    typeFromEnv e p' l' = fromMaybe (error $ "[ERROR] Can't find \"" ++ l' ++ "\".\n" ++ P.sourcePosPretty p') 
                $ M.lookup l e :: Type

-- TODO: #4 MonadError等でエラーを投げ得ることを明示する
-- TODO: #5 実装を整理する
-- 演繹によって"返り値の型"を導出する。行き詰まり状態になるとエラーを投げる
eval :: Env -> P.Term -> Type
eval env body = case body of
        P.Add p t t'      -> isNatWith env p t t'
        P.Sub p t t'      -> isNatWith env p t t'
        P.Mul p t t'      -> isNatWith env p t t' 
        P.Div p t t'      -> isNatWith env p t t'
        P.Eq p t t'       -> eq env p t t'
        P.Ne p t t'       -> eq env p t t'
        P.Gt p t t'       -> eq env p t t'
        P.Lt p t t'       -> eq env p t t'
        P.True  p         -> Type.Bool p
        P.False p         -> Type.Bool p
        P.Num   p _       -> Int p
        P.Label p l       -> getTypeFromEnv env p l
        P.If p b t t'     ->  case (isBool env b, eq env p t t') of
            (True , Type.Bool _) -> eval env t
            (False, Type.Bool _) -> error $ "[ERROR] " ++ show b ++ " is not Bool\n" ++ P.sourcePosPretty p
            (_    , _          ) -> error "[INTERNAL ERROR] Type.hs eval" 
        P.Let   _ exprs t -> let
            env' = M.union env $ globalEnv exprs :: Env
            in eval env' t
        P.App   p l ts    -> let
            t = getTypeFromEnv env p l
            in foldr (applyFunc . eval env) t ts
    where
    -- 二つのTermの型が一致するか確認する
    eq :: Env -> P.SourcePos -> P.Term -> P.Term -> Type
    eq e p t t' = let
        t1  = eval e t
        t1' = eval e t'
        in if t1 == t1' 
            then Type.Bool p
            else error $ "[ERROR] Type mismatch \""++show t++"\" and \""++show t' ++"\"\n" ++ P.sourcePosPretty p
    -- そのTermが数値型であるか
    -- TODO: #1
    isNat :: Env -> P.Term -> Bool
    isNat e t = case show $ eval e t of
        "Int" -> True
        _     -> False
    -- そのTermがBool型であるか
    -- TODO: #1
    isBool :: Env -> P.Term -> Bool
    isBool e t = case show $ eval e t of
        "Bool" -> True
        _      -> False
    -- TODO: #5 rename
    -- これ二つともInt?
    isNatWith :: Env -> P.SourcePos -> P.Term -> P.Term -> Type
    isNatWith e p t t' = case (isNat e t,  isNat e t') of
        (True,  True)  -> Int p
        (True,  False) -> error $ "[ERROR] Type mismatch: \""++show t' ++"\" is not Int\n" ++ P.sourcePosPretty p
        (False, True)  -> error $ "[ERROR] Type mismatch: \""++show t  ++"\" is not Int\n" ++ P.sourcePosPretty p
        (False, False) -> error $ "[ERROR] Type mismatch: \""++show t++"\" and \""++show t' ++"\" is not Int\n" ++ P.sourcePosPretty p
            

-- TypeDefより環境を生成
globalEnv :: [P.Expr] -> Env
globalEnv exprs = foldr globalEnv' M.empty
                $ filter isTypeDef exprs
          where
          globalEnv' :: P.Expr -> Env -> Env
          globalEnv' P.Define{} _  = error "ERROR: Type.hs, globalEnv"
          globalEnv' (P.TypeDef p n a) env = 
            if length a == 1
                then M.insert n (fromString p $ head a) env
                else M.insert n (Func p (map (fromString p) a)) env

          -- TODO: #3 ユーザ定義型を受け入れられるようにする
          fromString :: P.SourcePos -> String -> Type
          fromString p "Int"  = Int p 
          fromString p "Bool" = Bool p
          fromString p t      = error $ "[ERROR] \"" ++ show t ++ "\" is undefined\n" ++ show t++"\n"++ P.sourcePosPretty p

isTypeDef :: P.Expr -> Bool
isTypeDef P.TypeDef{} = Prelude.True
isTypeDef P.Define{}  = Prelude.False

applyFunc :: Type -> Type -> Type
applyFunc t (Int  p)    = error $ "[ERROR] \"Int\" is not apply " ++show t++"\n"++ P.sourcePosPretty p
applyFunc t (Bool p)    = error $ "[ERROR] \"Bool\" is not apply "++show t++"\n"++ P.sourcePosPretty p
applyFunc t (Func p ts) = case (head ts == t, length ts) of
    (True , 1) -> ts !! 1
    (True , _) -> Func p (tail ts)
    (False, _) -> error $ "[ERROR] type mismatch: " ++show t++" and "++show (head ts)++"\n"++ P.sourcePosPretty p

getTypeFromEnv :: Env -> P.SourcePos -> String -> Type
getTypeFromEnv e p label =
    fromMaybe (error $ "[ERROR] Can't find \"" ++ label ++ "\".\n" 
                    ++ P.sourcePosPretty p)
              $ M.lookup label e

-- 返り値の型を求める
retType :: Type -> Type
retType (Func _ x) = last x
retType x          = x


