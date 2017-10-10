{- |
Module      : Asm
Description : Asmを吐き出す
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

-}

module Asm
    ( AT.Asm(..)
    , AT.Tag(..)
    , emit
    ) where

import qualified Asm.Type as AT
import qualified Asm.Label as AL
import qualified Asm.Alloc as AA
import qualified Asm.Bin as AE

import qualified Expansion as E

emit :: [E.ABlock] -> [AE.BinAsm]
emit = AE.expandInstruction . AA.alloc . AL.toLabeledAsm 
