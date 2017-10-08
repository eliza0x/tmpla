{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Asm.Type
    ( Asm(..)
    , Tag(..)
    , Reg(..)
    , LabeledAsm
    , lvar
    , isContain
    ) where

import qualified Parser as S
import Type (Var(..), IntermediateForm(..))

data Asm a =
      Add   S.SourcePos a a a
    | Sub   S.SourcePos a a a
    | Mul   S.SourcePos a a a
    | Div   S.SourcePos a a a
    | Gt    S.SourcePos a a a
    | Lt    S.SourcePos a a a
    | Eq    S.SourcePos a a a
    | Ne    S.SourcePos a a a
    | Bof   S.SourcePos a a
    | Jot   S.SourcePos a
    | Push  S.SourcePos a
    | Pop   S.SourcePos a
    | Bind  S.SourcePos a Int
    | Label S.SourcePos a String
    | Num   S.SourcePos a Int
    | Sw    S.SourcePos a a Int
    | Lw    S.SourcePos a a Int
    | Addi  S.SourcePos a a Int
    | Subi  S.SourcePos a a Int
    deriving Eq

newtype Reg = Reg { fromReg :: Int }
    deriving (Show, Eq)

type LabeledAsm = Asm Var

instance IntermediateForm LabeledAsm where
    lvar (Add   _ a _ _)  = a
    lvar (Sub   _ a _ _)  = a
    lvar (Mul   _ a _ _)  = a
    lvar (Div   _ a _ _)  = a
    lvar (Gt    _ a _ _)  = a
    lvar (Lt    _ a _ _)  = a
    lvar (Eq    _ a _ _)  = a
    lvar (Ne    _ a _ _)  = a
    lvar (Bof   _ a _  )  = a
    lvar (Jot   _ a    )  = a
    lvar (Push  _ a    )  = a
    lvar (Pop   _ a    )  = a
    lvar (Bind  _ a _  )  = a
    lvar (Label _ a _  )  = a
    lvar (Num   _ a _  )  = a
    lvar (Sw    _ a _ _ ) = a
    lvar (Lw    _ a _ _ ) = a
    lvar (Addi  _ a _ _ ) = a
    lvar (Subi  _ a _ _ ) = a

    isContain v (Addi  _ _ a _) = v == a
    isContain v (Subi  _ _ a _) = v == a
    isContain v (Sw    _ _ a _) = v == a
    isContain v (Lw    _ _ a _) = v == a
    isContain v (Add   _ _ a b) = v == a || v == b
    isContain v (Sub   _ _ a b) = v == a || v == b
    isContain v (Mul   _ _ a b) = v == a || v == b
    isContain v (Div   _ _ a b) = v == a || v == b
    isContain v (Gt    _ _ a b) = v == a || v == b
    isContain v (Lt    _ _ a b) = v == a || v == b
    isContain v (Eq    _ _ a b) = v == a || v == b
    isContain v (Ne    _ _ a b) = v == a || v == b
    isContain v (Bof   _ _ a  ) = v == a
    isContain _ Bind  {}        = False
    isContain _ Jot   {}        = False 
    isContain _ Push  {}        = False 
    isContain _ Pop   {}        = False 
    isContain _ Label {}        = False 
    isContain _ Num   {}        = False 

instance (Show a) => Show (Asm a) where
    show (Add   _ a b c) = show a ++ " := " ++ show b ++ " + " ++ show c ++ ";"
    show (Sub   _ a b c) = show a ++ " := " ++ show b ++ " - " ++ show c ++ ";"
    show (Mul   _ a b c) = show a ++ " := " ++ show b ++ " * " ++ show c ++ ";"
    show (Div   _ a b c) = show a ++ " := " ++ show b ++ " / " ++ show c ++ ";"
    show (Gt    _ a b c) = show a ++ " := " ++ show b ++ " > " ++ show c ++ ";"
    show (Lt    _ a b c) = show a ++ " := " ++ show b ++ " < " ++ show c ++ ";"
    show (Eq    _ a b c) = show a ++ " := " ++ show b ++ " = " ++ show c ++ ";"
    show (Ne    _ a b c) = show a ++ " := " ++ show b ++ " /= " ++ show c ++ ";"
    show (Bof   _ a b  ) = "if (not "++show a++") {\n"++unlines (map ("    "++) $ lines $ show b)++"}"
    show (Jot   _ a    ) = "jot ("++show a++")"
    show (Push  _ a    ) = "push ("++show a++")"
    show (Pop   _ a    ) = show a ++ " := pop()"
    show (Bind  _ a b  ) = show a ++ " := " ++ show b
    show (Label _ a l  ) = show a ++ " := " ++ l
    show (Num   _ a n  ) = show a ++ " := " ++ show n
    show (Sw    _ a b n) = show a ++ " := " ++ show b ++ "[" ++ show n ++ "];"
    show (Lw    _ a b n) = show a ++ " := " ++ show b ++ "[" ++ show n ++ "];"
    show (Addi  _ a b n) = show a ++ " := " ++ show b ++ " + " ++ show n ++ ";"
    show (Subi  _ a b n) = show a ++ " := " ++ show b ++ " - " ++ show n ++ ";"
 
data Tag a = Tag Var
           | Data a
    deriving Eq

instance (Show a) => Show (Tag a) where
    show (Tag v) = "\n" ++ show v ++ ":"
    show (Data a) = "    " ++ show a

