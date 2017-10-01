{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Parser
Description : ソースコードを木に変換する
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

ソースコードを木構造に変換する。
-}

module Parser
    ( parser
    , Expr(..)
    , Term(..)
    , Type
    , SourcePos(..)
    , sourcePosPretty
    , getPos
    , mkPos
    ) where

import Text.Megaparsec hiding (label, Label)
import Prelude hiding (div) 

data Expr = Define { pos  :: SourcePos
                   , name :: String
                   , args :: [String]
                   , body :: Term } 
          | TypeDef { pos  :: SourcePos
                    , name :: String
                    , args :: [String]
                    } deriving Eq

data Term = Add    SourcePos Term Term
          | Sub    SourcePos Term Term
          | Mul    SourcePos Term Term
          | Div    SourcePos Term Term
          | App    SourcePos String [Term]
          | Eq     SourcePos Term Term
          | Ne     SourcePos Term Term
          | Gt     SourcePos Term Term
          | Lt     SourcePos Term Term
          | If     SourcePos Term Term Term
          | True   SourcePos
          | False  SourcePos
          | Num    SourcePos Int
          | Label  SourcePos String
          | Let    SourcePos [Expr] Term
          | Lambda SourcePos String Term
          deriving (Eq)

type Type = String

instance Show Expr where
    show (Define _ n as b) = "Define " ++ n ++ " " ++ show as ++ " " ++ show b
    show (TypeDef _ n as)  = "TypeDef " ++ n ++ " " ++ show as

instance Show Term where
    show (Add _ t t')     = "Add " ++ show t ++ " " ++ show t'
    show (Sub _ t t')     = "Sub " ++ show t ++ " " ++ show t'
    show (Mul _ t t')     = "Mul " ++ show t ++ " " ++ show t'
    show (Div _ t t')     = "Div " ++ show t ++ " " ++ show t'
    show (App _ l ts)     = "App " ++ l ++ " " ++ show ts
    show (Eq  _ t t')     = "Eq  " ++ show t ++ " " ++ show t'
    show (Ne  _ t t')     = "Ne  " ++ show t ++ " " ++ show t'
    show (Gt  _ t t')     = "Gt  " ++ show t ++ " " ++ show t'
    show (Lt  _ t t')     = "Lt  " ++ show t ++ " " ++ show t'
    show (If  _ b t t')   = "If  " ++ show b ++ " " ++ show t ++ " " ++ show t'
    show (Parser.True  _) = "True"
    show (Parser.False _) = "False"
    show (Num _ n)        = show n
    show (Label _ l)      = l
    show (Let _ exprs t)  = "Let " ++ show exprs ++ show t
    show (Lambda _ l tr)   = "\\" ++ show l ++ " -> " ++ show tr

getPos :: Term -> SourcePos
getPos (Add   p _ _)    = p
getPos (Sub   p _ _)    = p
getPos (Mul   p _ _)    = p
getPos (Div   p _ _)    = p
getPos (App   p _ _)    = p
getPos (Eq    p _ _)    = p
getPos (Ne    p _ _)    = p
getPos (Gt    p _ _)    = p
getPos (Lt    p _ _)    = p
getPos (If    p _ _ _)  = p
getPos (Parser.True  p) = p
getPos (Parser.False p) = p
getPos (Num   p _)      = p
getPos (Label p _)      = p
getPos (Let   p _ _)    = p
getPos (Lambda p _ _)   = p

type TmplaParser = Parsec Dec String

parser :: FilePath -> String -> [Expr]
parser filename = either (error . parseErrorPretty) id 
       . parse (expr <* eof) filename

expr :: TmplaParser [Expr]
expr = some (try define <|> typeDef)

define :: TmplaParser Expr
define = space
     *>  (Define 
     <$> getPosition
     <*> lowerWord
     <*> (many lowerWord <* string ":=" <* space)
     <*> (term <* char ';'))
     <* space

term :: TmplaParser Term
term = try (do
    p <- getPosition
    t <- term'
    op <- equal <|> notEqual <|> greaterThan <|> lowerThan
    t' <- term
    return $ op p t t') <|> term'
    where
    equal       = return Eq <* char '='
    notEqual    = return Ne <* string "/="
    greaterThan = return Gt <* string ">"
    lowerThan   = return Lt <* string "<"

term' :: TmplaParser Term
term' = try (do
    t <- term''
    op <- add <|> sub
    t' <- term'
    return $ op pos t t') <|> term''
    where
    add = return . Add <$> (getPosition <* char '+')
    sub = return . Sub <$> (getPosition <* char '-')

term'' :: TmplaParser Term
term'' = try (do
    t <- term'''
    op <- mul <|> div
    t' <- term''
    return $ op pos t t') <|> term'''
    where
    mul = return . Mul <$> (getPosition <* char '*')
    div = return . Div <$> (getPosition <* char '/')

term''' :: TmplaParser Term
term''' = space *> ( brace 
                 <|> lambda
                 <|> letIn
                 <|> ifParser
                 <|> app 
                 <|> label
                 <|> bool
                 <|> num
                   ) <* space
    where
    letIn :: TmplaParser Term
    letIn = do
        p <- getPosition
        defs <- string "let" *> expr <* string "in"
        t <- term
        return $ Let p defs t

    app :: TmplaParser Term
    app = try $ App <$> getPosition <*> lowerWord <*> (some term <* space)

    lambda :: TmplaParser Term
    lambda = do
        p <- getPosition
        as <- char '\\' *> many (space *> lowerWord) <* space <* string "->" <* space
        t <- term
        return $ foldr (Lambda p) t as 
    
    brace :: TmplaParser Term
    brace = between (char '(') (char ')') term

    ifParser :: TmplaParser Term
    ifParser = do
        p <- getPosition <* space <* string "if" <* space
        t   <- term <* string "then" <* space
        t'  <- term <* string "else" <* space
        t'' <- term
        return $ If p t t' t''
   
    bool :: TmplaParser Term
    bool = do
        p <- getPosition <* space
        t <- true <|> false
        return $ t p
        where
        true  = string "true" *> return Parser.True
        false = string "false" *> return Parser.False

label :: TmplaParser Term
label = Label <$> getPosition <*> lowerWord

typeDef :: TmplaParser Expr
typeDef = space
        *>  (TypeDef 
        <$> getPosition
        <*> (lowerWord <* string "::")
        <*> argsParser)
        <* space

argsParser :: TmplaParser [String]
argsParser = do
    h <- upperWord
    t <- many (string "->" *> upperWord)
    return $ h:t

upperWord :: TmplaParser String
upperWord = word upperChar

lowerWord :: TmplaParser String
lowerWord = word lowerChar

word :: TmplaParser Char -> TmplaParser String
word p = do
    space
    notFollowedBy reservedWord
    h <- p
    t <- many alphaNumChar
    space
    return $ h:t
    where
    reservedWord :: TmplaParser String
    reservedWord = string ":="
               <|> string ";"
               <|> string "{-"
               <|> string "-}"
               <|> string "let"
               <|> string "in"
               <|> string "if"
               <|> string "then"
               <|> string "else"
               <|> string "true"
               <|> string "false"

num :: TmplaParser Term
num = Num <$> getPosition <*> (read <$> (space *> some digitChar <* space))
