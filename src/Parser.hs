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
    , P.Expr(..)
    , P.Term(..)
    , P.Type
    , P.SourcePos(..)
    , P.sourcePosPretty
    , P.getPos
    , P.mkPos
    ) where

import Text.Megaparsec hiding (label, Label)
import Prelude hiding (div) 
import qualified Parser.Type as P
import Parser.Type hiding (term, label, num)

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
    app = try $ App <$> getPosition <*> label <*> (some term <* space)

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
        true  = string "true" *> return  Parser.Type.True
        false = string "false" *> return Parser.Type.False

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
