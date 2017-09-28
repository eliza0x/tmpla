{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parser
    , Expr(..)
    , Term(..)
    ) where

import Text.Megaparsec hiding (label, Label)
import Prelude hiding (div) 

data Expr = Define { pos  :: SourcePos
                   , name :: String
                   , args :: [String]
                   , body :: Term
                   } deriving (Show, Eq)

data Term = Add   SourcePos Term Term
          | Sub   SourcePos Term Term
          | Mul   SourcePos Term Term
          | Div   SourcePos Term Term
          | Call  SourcePos Term [Term]
          | Eq    SourcePos Term Term
          | Ne    SourcePos Term Term
          | If    SourcePos Term Term   Term
          | True  SourcePos
          | False SourcePos
          | Num   SourcePos Int
          | Label SourcePos String
          deriving (Show, Eq)

type TmplaParser = Parsec Dec String

parser :: String -> [Expr]
parser = either (error . parseErrorPretty) id 
       . parse expr "Parser.hs"

expr :: TmplaParser [Expr]
expr = some define <* eof

define :: TmplaParser Expr
define = space
     *>  (Define 
     <$> getPosition
     <*> word
     <*> (many word <* string ":=" <* space)
     <*> (term <* char ';'))
     <* space

term :: TmplaParser Term
term = try (do
    pos <- getPosition
    t <- term'
    op <- equal <|> notEqual
    t' <- term
    return $ op pos t t') <|> term'
    where
    equal = return Eq <* char '='
    notEqual = return Eq <* string "/="

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
                 <|> ifParser
                 <|> call 
                 <|> label
                 <|> bool
                 <|> num
                   ) <* space
    where
    call :: TmplaParser Term
    call = try $ do
        pos <- getPosition
        l <- label
        args <- some term <* space
        return $ Call pos l args
   
    brace :: TmplaParser Term
    brace = between (char '(') (char ')') term

    ifParser :: TmplaParser Term
    ifParser = do
        pos <- getPosition <* space <* string "if" <* space
        t   <- term <* string "then" <* space
        t'  <- term <* string "else" <* space
        t'' <- term
        return $ If pos t t' t''
   
    bool :: TmplaParser Term
    bool = do
        pos <- getPosition <* space
        t <- true <|> false
        return $ t pos
        where
        true  = string "true" *> return Parser.True
        false = string "false" *> return Parser.False

label :: TmplaParser Term
label = Label <$> getPosition <*> word

word :: TmplaParser String
word = do
    space
    notFollowedBy reservedWord
    h <- lowerChar
    t <- many alphaNumChar
    space
    return $ h:t
    where
    reservedWord :: TmplaParser String
    reservedWord = string ":="
               <|> string ";"
               <|> string "if"
               <|> string "then"
               <|> string "else"
               <|> string "true"
               <|> string "false"

num :: TmplaParser Term
num = Num <$> getPosition <*> (read <$> (space *> some digitChar <* space))
