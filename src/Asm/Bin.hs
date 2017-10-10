module Asm.Bin where

import qualified Parser as S
import qualified Asm.Type as AT
import Type (Var(..))

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Numeric as N
import qualified Data.Char as C

data BinAsm =
      Add  S.SourcePos Int Int Int
    | Sub  S.SourcePos Int Int Int
    | Mul  S.SourcePos Int Int Int
    | Div  S.SourcePos Int Int Int
    | Gt   S.SourcePos Int Int Int
    | Lt   S.SourcePos Int Int Int
    | Eq   S.SourcePos Int Int Int
    | Ne   S.SourcePos Int Int Int
    | Bof  S.SourcePos Int Int
    | Jot  S.SourcePos Int
    | Bind S.SourcePos Int Int
    | Sw   S.SourcePos Int Int Int
    | Lw   S.SourcePos Int Int Int
    | Addi S.SourcePos Int Int Int
    | Subi S.SourcePos Int Int Int
    deriving Eq

instance Show BinAsm where
    show (Add _ a b c)  = "000000" ++ concatMap to6digitBin [a, b, c]
    show (Sub _ a b c)  = "000001" ++ concatMap to6digitBin [a, b, c]
    show (Mul _ a b c)  = "000010" ++ concatMap to6digitBin [a, b, c]
    show Div{}          = replicate 24 '0'
    show (Gt  _ a b c)  = "000100" ++ concatMap to6digitBin [a, b, c]
    show (Lt  _ a b c)  = "000100" ++ concatMap to6digitBin [a, c, b]
    show (Eq  _ a b c)  = "000111" ++ concatMap to6digitBin [a, b, c]
    show (Ne  _ a b c)  = "001000" ++ concatMap to6digitBin [a, b, c]
    show Bof{}          = undefined -- TODO: CPUにBofを定義する
    show (Sw  _ a b n)  = "011100" ++ concatMap to6digitBin [a, b, n]
    show (Lw  _ a b n)  = "011101" ++ concatMap to6digitBin [a, b, n]
    show (Addi _ a b n) = "010000" ++ concatMap to6digitBin [a, b, n]
    show (Subi _ a b n) = "010001" ++ concatMap to6digitBin [a, b, n]
    show (Bind _ a n)   = "0000" ++ alignment 6 (toBin a)
                                 ++ alignment 14 (toBin n)

to6digitBin :: Int -> String
to6digitBin = alignment 6 . toBin

toBin :: Int -> String
toBin n = N.showIntAtBase 2 C.intToDigit n ""

alignment :: Int -> String -> String
alignment n str = replicate (n - length str) '0' ++  str

expandInstruction :: [AT.Tag (AT.Asm AT.Reg)] -> [BinAsm]
expandInstruction tags = toBinAsm $ concatMap (\tag -> case tag of
    AT.Tag a  -> [AT.Tag a]
    AT.Data b -> map  AT.Data $ expandInstruction' b) tags

expandInstruction' :: AT.Asm AT.Reg -> [AT.Asm AT.Reg]
expandInstruction' asm = case asm of
    AT.Push  pos a     -> [AT.Subi pos (AT.Reg 30) (AT.Reg 30) 1, AT.Sw pos a (AT.Reg 30) 0]
    AT.Pop   pos a     -> [AT.Lw pos a (AT.Reg 30) 0, AT.Addi pos (AT.Reg 30) (AT.Reg 30) 0]
    AT.Num   pos a n   -> [AT.Bind pos a n]
    x                  -> [x]

toBinAsm :: [AT.Tag (AT.Asm AT.Reg)] -> [BinAsm]
toBinAsm exprs = let
    labelAddrDict = inspectLabelAddr exprs :: M.Map Var Int
    exprs'    = concatMap (\tag -> case tag of
        AT.Tag _ -> []
        AT.Data a -> [a]) exprs
    in map (conv labelAddrDict) exprs'

conv :: M.Map Var Int -> AT.Asm AT.Reg -> BinAsm
conv dict asm = case asm of
    AT.Add   pos a b c -> Add pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Sub   pos a b c -> Sub pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Mul   pos a b c -> Mul pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Div   pos a b c -> Div pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Gt    pos a b c -> Gt  pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Lt    pos a b c -> Lt  pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Eq    pos a b c -> Eq  pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Ne    pos a b c -> Ne  pos (AT.fromReg a) (AT.fromReg b) (AT.fromReg  c)
    AT.Bof   pos a b   -> Bof pos (AT.fromReg a) (AT.fromReg b)
    AT.Jot   pos a     -> Jot pos (AT.fromReg a)
    AT.Bind  pos a n   -> Bind pos (AT.fromReg a) n
    AT.Label pos a l   -> Bind pos (AT.fromReg a) (dict ! Var l)
    AT.Sw    pos a b n -> Sw pos (AT.fromReg a) (AT.fromReg b) n
    AT.Lw    pos a b n -> Lw pos (AT.fromReg a) (AT.fromReg b) n
    AT.Addi  pos a b n -> Addi pos (AT.fromReg a) (AT.fromReg b) n
    AT.Subi  pos a b n -> Subi pos (AT.fromReg a) (AT.fromReg b) n
    AT.Num{}           -> undefined
    AT.Push{}          -> undefined
    AT.Pop{}           -> undefined

inspectLabelAddr :: [AT.Tag(AT.Asm AT.Reg)] -> M.Map Var Int
inspectLabelAddr tags = inspectLabelAddr' 0 tags M.empty

inspectLabelAddr' :: Int -> [AT.Tag (AT.Asm AT.Reg)] -> M.Map Var Int -> M.Map Var Int
inspectLabelAddr' pc (x:xs) dict = inspectLabelAddr' (pc+1) xs $ (case x of
    AT.Tag l -> M.insert l pc
    AT.Data _ -> id) dict
