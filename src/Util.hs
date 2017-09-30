module Util
    ( genUUID
    ) where

import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

genUUID :: IO String
genUUID = ('_':) . U.toString <$> U.nextRandom

