module Typing.TypeEq where

import           Typing.QType

data TypeEq = SumSizeEq [QType] QType
        | AtLeastSizeEq [QType] QType
        | IsQubits QType
        | IsMeasuredQubits QType
        | TypeEq QType QType
      deriving (Show)
