{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parser
    , Expr(..)
    , Term(..)
    ) where

import Text.Megaparsec hiding (label, Label)

data Expr = Define String [String] Term
          deriving (Show, Eq)

data Term = Add Term Term
          | Sub Term Term
          | Mul Term Term
          | Div Term Term
          | Num Int
          | Label String
          | Call String [String]
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
     <$> label 
     <*> (many label <* string ":=" <* space)
     <*> (term <* char ';'))
     <* space

term :: TmplaParser Term
term = space *> (try add <|> try sub <|> term') <* space
    where
    add = Add <$> (term' <* char '+' <* space) <*> term 
    sub = Sub <$> (term' <* char '-' <* space) <*> term

term' :: TmplaParser Term
term' = space *> (try mul <|> try div <|> term'') <* space
    where
    mul = Mul <$> (term'' <* char '*' <* space) <*> term'
    div = Div <$> (term'' <* char '/' <* space) <*> term'

term'' :: TmplaParser Term
term'' = space *> (try call <|> try brace <|> try (Label <$> label) <|> (Num <$> num)) <* space
    where
    call :: TmplaParser Term
    call = Call <$> label <*> 
            ( space *> char '(' *>
                (args <|> return [])
            <* space <* char ')')
        where 
        args :: TmplaParser [String]
        args = (:) <$> (space *> label) <*> (space *> many (char ',' *> space *> label <* space))
    
    brace :: TmplaParser Term
    brace = space *> char '(' *> space *> term <* space <* char ')' <* space
    
    num :: TmplaParser Int
    num = read <$> (space *> some digitChar <* space)

label :: TmplaParser String
label = do
    space
    h <- lowerChar
    t <- many alphaNumChar
    space
    return $ h:t

