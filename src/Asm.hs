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
    , AL.LabeledAsm
    , AL.toLabeledAsm
    , AA.alloc
    , AE.expandInstruction
    ) where

import qualified Asm.Type as AT
import qualified Asm.Label as AL
import qualified Asm.Alloc as AA
import qualified Asm.Expand as AE

