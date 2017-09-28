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
expr = space *> some define <* space <* eof

define :: TmplaParser Expr
define = space
     *>  (Define 
     <$> word
     <*> (many word <* string ":=" <* space)
     <*> (term <* char ';'))
     <* space

term :: TmplaParser Term
term = space *> ( try equal <|> try notEqual <|> term' ) <* space
    where
    equal = Eq <$> (term' <* char '=' <* space) <*> term
    notEqual = Ne <$> (term' <* string "/=" <* space) <*> term

term' :: TmplaParser Term
term' = space *> (try add <|> try sub <|> term'') <* space
    where
    add = Add <$> (term'' <* char '+' <* space) <*> term'
    sub = Sub <$> (term'' <* char '-' <* space) <*> term'

term'' :: TmplaParser Term
term'' = space *> (try mul <|> try div <|> term''') <* space
    where
    mul = Mul <$> (term''' <* char '*' <* space) <*> term''
    div = Div <$> (term''' <* char '/' <* space) <*> term''

term''' :: TmplaParser Term
term''' = space *> ( brace 
                 <|> ifParser
                 <|> try call 
                 <|> try num
                 <|> try true
                 <|> try false
                   ) <* space
    where
    call :: TmplaParser Term
    call = Call <$> label <*> (many (space *> term) <* space)
   
    brace :: TmplaParser Term
    brace = between (char '(') (char ')') term

    ifParser :: TmplaParser Term
    ifParser = do
        _ <- space *> string "if" <* space
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
               <|> string "if"
               <|> string "then"
               <|> string "else"
               <|> string "true"
               <|> string "false"

num :: TmplaParser Term
num = Num . read <$> (space *> some digitChar <* space)

