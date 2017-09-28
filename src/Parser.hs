{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parser
    , Expr(..)
    , Term(..)
    ) where

import Text.Megaparsec hiding (label, Label)
import Prelude hiding (div) 

data Expr = Define String [String] Term
          deriving (Show, Eq)

data Term = Add Term Term
          | Sub Term Term
          | Mul Term Term
          | Div Term Term
          | Call Term [Term]
          | Eq Term Term
          | Ne Term Term
          | If Term Term Term
          | True
          | False
          | Num Int
          | Label String
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
     <$> word
     <*> (many word <* string ":=" <* space)
     <*> (term <* char ';'))
     <* space

term :: TmplaParser Term
term = try (do
    t <- term'
    op <- equal <|> notEqual
    t' <- term
    return $ op t t') <|> term'
    where
    equal = return Eq <* char '='
    notEqual = return Eq <* string "/="

{-
term = space *> (equal <|> notEqual <|> term' ) <* space
    where
    equal    = Eq <$> (try (term' <* char '=') <* space)    <*> term <?> "equal"
    notEqual = Ne <$> (try (term' <* string "/=") <* space) <*> term <?> "not equal"
-}

term' :: TmplaParser Term
term' = try (do
    t <- term''
    op <- add <|> sub
    t' <- term'
    return $ op t t') <|> term''
    where
    add = return Add <* char '+'
    sub = return Sub <* char '-'

term'' :: TmplaParser Term
term'' = try (do
    t <- term'''
    op <- mul <|> div
    t' <- term''
    return $ op t t') <|> term'''
    where
    mul = return Mul <* char '*'
    div = return Div <* char '/'

term''' :: TmplaParser Term
term''' = space *> ( brace 
                 <|> ifParser
                 <|> call 
                 <|> label
                 <|> true
                 <|> false
                 <|> num
                   ) <* space
    where
    call :: TmplaParser Term
    call = try $ do
        l <- label
        args <- some term <* space
        return $ Call l args
   
    brace :: TmplaParser Term
    brace = between (char '(') (char ')') term

    ifParser :: TmplaParser Term
    ifParser = do
        _  <- space *> string "if"   <* space
        t   <- term <* string "then" <* space
        t'  <- term <* string "else" <* space
        t'' <- term
        return $ If t t' t''
   
    true :: TmplaParser Term
    true = do
        _ <- space *> string "true" <* space
        return Parser.True

    false :: TmplaParser Term
    false = do
        _ <- space *> string "false" <* space
        return Parser.False

label :: TmplaParser Term
label = Label <$> word

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
num = Num . read <$> (space *> some digitChar <* space)

