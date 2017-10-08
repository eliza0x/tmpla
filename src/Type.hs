{- |
Module      : Type
Description : 型
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental

共通して利用する型を提供する
-}

module Type (
      Var(..)
    ) where

newtype Var = Var { fromVar ::  String }
    deriving (Eq, Ord)

instance Show Var where
    show (Var l) = l


