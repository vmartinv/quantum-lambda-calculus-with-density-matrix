module Typing.TypeEq where

import           Typing.QType

data TypeEq = SumSizeEq [QType] QType
        | AtLeastSizeEq [QType] Int
        | EqualTypeEq QType QType
      deriving (Show, Eq)
