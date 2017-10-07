module Asm.Label
    ( toLabeledAsm
    , LabeledAsm
    )where

import qualified KNormal as K
import qualified ANormal as A

import Asm.Type

type LabeledAsm = Asm K.Var

toLabeledAsm :: [A.ABlock] -> [Tag LabeledAsm]
toLabeledAsm = concatMap
    (\(A.ABlock _ n b) -> let
        anorm = Tag n : map Data b :: [Tag A.ANormal]
        in concatMap labeledAsm anorm)

labeledAsm :: Tag A.ANormal -> [Tag LabeledAsm]
labeledAsm (Tag  v) = [Tag v]
labeledAsm (Data body) = case body of
    A.Add    p a b c   -> [Data $ Add p a b c]
    A.Sub    p a b c   -> [Data $ Sub p a b c]
    A.Mul    p a b c   -> [Data $ Mul p a b c]
    A.Div    p a b c   -> [Data $ Div p a b c]
    A.Eq     p a b c   -> [Data $ Eq  p a b c]
    A.Ne     p a b c   -> [Data $ Ne  p a b c]
    A.Gt     p a b c   -> [Data $ Gt  p a b c]
    A.Lt     p a b c   -> [Data $ Lt  p a b c]
    A.Num    p a n     -> [Data $ Num p a n]
    A.Label  p a s     -> [Data $ Label p a s]
    A.Jot    p a       -> [Data $ Jot p a]
    A.Lambda p a norms -> (Data $ Pop p a) : concatMap (labeledAsm . Data) norms 
    A.Call   p a vars  -> map (Data . Push p) vars ++ [Data $ Jot p a]
    A.Bne    p _ bnorms tnorms (A.CallElse elseAddr continueAddr _) -> let
        basm = concatMap (labeledAsm . Data) bnorms 
        tasm = concatMap (labeledAsm . Data) tnorms 
        continueTag = Tag continueAddr
        in basm ++ [Data $ Bof p (K.Var "_return") elseAddr] ++ tasm ++ [continueTag]
