module Asm.Alloc where

import qualified Asm.Type as AT
import qualified Asm.Label as AL
import Type (Var(..))

import qualified Data.List as L
import qualified Control.Monad.Writer.Strict as W
import qualified Data.Maybe as M
import qualified Data.Map.Strict as M
import qualified Control.Arrow as Arr
import Control.Applicative ((<|>))
import Data.Maybe

newtype Reg = Reg { fromReg :: Int }
    deriving (Show, Eq)

data Life =
      Live Int
    | Dead 
    deriving (Show, Eq)

predLife :: Life -> Life
predLife (Live 0) = Dead
predLife (Live n) = Live $ n - 1
predLife Dead     = Dead 

openReg :: [(Int, Life)] -> Int
openReg = fst
        . fromMaybe (error "[INTERNAL ERROR]")
        <$> L.find (\(_, life) -> case life of
        Live _ -> False
        Dead   -> True)

occupetionReg :: Int -- ^ レジスタ番号
              -> Int -- ^ 寿命
              -> [(Int, Life)] 
              -> [(Int, Life)] 
occupetionReg = update 
    where
    update :: Int -> Int -> [(Int, Life)] -> [(Int, Life)]
    update _ _ [] = []
    update k v ((n, m):l) = if k == n then (k, Live v) : update k v l
                                      else (n, m) : update k v l

data AfterSecond a = FirstTime { body :: a }
                   | AfterSecond { body :: a }
    deriving (Show, Eq)

alloc :: [AT.Tag AL.LabeledAsm] -> [AT.Tag (AT.Asm Reg)]
alloc lasms = let
    untag :: AT.Tag a -> [a]
    untag (AT.Data b) = [b]
    untag (AT.Tag _)  = []

    lasms' = map FirstTime $ concatMap untag lasms
    regs = map (\n -> (n, Dead)) [0..28]
    in conv lasms (M.fromList $ W.execWriter $ alloc' lasms' regs)

conv :: [AT.Tag AL.LabeledAsm] -> M.Map Var Reg -> [AT.Tag (AT.Asm Reg)]
conv lasms dict = map (\lasm -> case lasm of 
    AT.Tag a  -> AT.Tag a
    AT.Data b -> AT.Data $ conv' dict b) lasms 

conv' :: M.Map Var Reg -> AL.LabeledAsm -> AT.Asm Reg
conv' dict lasm = case lasm of
    AT.Addi  pos a b n -> AT.Addi  pos (access a dict) (access b dict) n 
    AT.Subi  pos a b n -> AT.Subi  pos (access a dict) (access b dict) n 
    AT.Add   pos a b c -> AT.Add   pos (access a dict) (access b dict) (access c dict)
    AT.Sub   pos a b c -> AT.Sub   pos (access a dict) (access b dict) (access c dict)
    AT.Mul   pos a b c -> AT.Mul   pos (access a dict) (access b dict) (access c dict)
    AT.Div   pos a b c -> AT.Div   pos (access a dict) (access b dict) (access c dict)
    AT.Gt    pos a b c -> AT.Gt    pos (access a dict) (access b dict) (access c dict)
    AT.Lt    pos a b c -> AT.Lt    pos (access a dict) (access b dict) (access c dict)
    AT.Eq    pos a b c -> AT.Eq    pos (access a dict) (access b dict) (access c dict)
    AT.Ne    pos a b c -> AT.Ne    pos (access a dict) (access b dict) (access c dict)
    AT.Sw    pos a b n -> AT.Sw    pos (access a dict) (access b dict) n
    AT.Lw    pos a b n -> AT.Lw    pos (access a dict) (access b dict) n
    AT.Bof   pos a b   -> AT.Bof   pos (access a dict) (access b dict) 
    AT.Jot   pos a     -> AT.Jot   pos (access a dict)
    AT.Push  pos a     -> AT.Push  pos (access a dict)
    AT.Pop   pos a     -> AT.Pop   pos (access a dict)
    AT.Bind  pos a x   -> AT.Bind  pos (access a dict) x
    AT.Label pos a l   -> AT.Label pos (access a dict) l
    AT.Num   pos a n   -> AT.Num   pos (access a dict) n
    where
    access :: Var -> M.Map Var Reg -> Reg
    access key dict' = M.fromMaybe (error "[INTERNAL ERROR]") $ M.lookup key dict'

alloc' :: [AfterSecond AL.LabeledAsm] -> [(Int, Life)] -> W.Writer [(Var, Reg)] ()
alloc' [] _ = return ()
alloc' (AfterSecond _:xs) lifetimes = alloc' xs (map (Arr.second predLife ) lifetimes)
alloc' (x:xs) lifetimes = do
    -- レジスタを必要としている変数の名前を調べる
    let lvar = AT.lVar $ body x
    -- レジスタの生存期間を調べる
    let distance = M.fromMaybe 0 $ lastAppear 1 lvar xs
    -- 以降の同名の変数は検査済みとする
    let xs' = alreadyExsists (AT.lVar $ body x) xs :: [AfterSecond AL.LabeledAsm]
    let reg = Reg $ openReg lifetimes
    W.tell [(lvar, reg)]
    let lifetimes' = map (Arr.second predLife)
            $ occupetionReg (fromReg reg) distance lifetimes
    alloc' xs' lifetimes'

lastAppear :: Int -> Var ->  [AfterSecond AL.LabeledAsm] -> Maybe Int
lastAppear _ _ [] = Nothing
lastAppear distance var (a:lasms) = if isContain var (body a)
    then lastAppear (distance+1) var lasms <|> Just distance
    else lastAppear (distance+1) var lasms

alreadyExsists :: Var -> [AfterSecond AL.LabeledAsm] -> [AfterSecond AL.LabeledAsm] 
alreadyExsists _ [] = []
alreadyExsists var (AfterSecond b:xs) = AfterSecond b : alreadyExsists var xs
alreadyExsists var (FirstTime b:xs) = (if var == AT.lVar b 
    then AfterSecond b   else FirstTime b) : alreadyExsists var xs

isContain :: Var -> AL.LabeledAsm -> Bool
isContain v (AT.Addi  _ _ a _) = v == a
isContain v (AT.Subi  _ _ a _) = v == a
isContain v (AT.Sw    _ _ a _) = v == a
isContain v (AT.Lw    _ _ a _) = v == a
isContain v (AT.Add   _ _ a b) = v == a || v == b
isContain v (AT.Sub   _ _ a b) = v == a || v == b
isContain v (AT.Mul   _ _ a b) = v == a || v == b
isContain v (AT.Div   _ _ a b) = v == a || v == b
isContain v (AT.Gt    _ _ a b) = v == a || v == b
isContain v (AT.Lt    _ _ a b) = v == a || v == b
isContain v (AT.Eq    _ _ a b) = v == a || v == b
isContain v (AT.Ne    _ _ a b) = v == a || v == b
isContain v (AT.Bof   _ _ a  ) = v == a
isContain _ AT.Bind  {}        = False
isContain _ AT.Jot   {}        = False 
isContain _ AT.Push  {}        = False 
isContain _ AT.Pop   {}        = False 
isContain _ AT.Label {}        = False 
isContain _ AT.Num   {}        = False 

