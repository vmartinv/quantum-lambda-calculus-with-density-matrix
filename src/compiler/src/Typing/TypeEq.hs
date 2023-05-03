module Typing.TypeEq where

import           Typing.QType

data TypeEq = SumSizeEq [QType] QType
        | AtLeastSizeEq [QType] QType
        | EqualTypeEq QType QType
      deriving (Show)
