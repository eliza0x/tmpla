module Asm.Type
    ( Asm(..)
    , Tag(..)
    , lVar
    ) where

import qualified Parser as S
import Type (Var(..))

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

lVar :: Asm a -> a
lVar (Add   _ a _ _) = a
lVar (Sub   _ a _ _) = a
lVar (Mul   _ a _ _) = a
lVar (Div   _ a _ _) = a
lVar (Gt    _ a _ _) = a
lVar (Lt    _ a _ _) = a
lVar (Eq    _ a _ _) = a
lVar (Ne    _ a _ _) = a
lVar (Bof   _ a _  ) = a
lVar (Jot   _ a    ) = a
lVar (Push  _ a    ) = a
lVar (Pop   _ a    ) = a
lVar (Bind  _ a _  ) = a
lVar (Label _ a _  ) = a
lVar (Num   _ a _  ) = a
lVar (Sw    _ a _ _ ) = a
lVar (Lw    _ a _ _ ) = a
lVar (Addi  _ a _ _ ) = a
lVar (Subi  _ a _ _ ) = a

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

