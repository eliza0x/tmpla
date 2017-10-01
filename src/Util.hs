{- |
Module      : Util
Description : 便利ツールズ
Copyright   : (c) eliza0x, 2017
License     : MIT
Maintainer  : me@eliza.link
Stability   : experimental
-}

module Util
    ( genUUID
    ) where

import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

genUUID :: IO String
genUUID = ('_':) . U.toString <$> U.nextRandom

