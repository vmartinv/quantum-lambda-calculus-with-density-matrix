module Typing.TypeError where

import           Control.Monad.Except
import           Data.Complex
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Typing.QType

data TypeError  = UnificationFail QType QType
              | InfiniteType VariableId QType
              | UnboundVariable T.Text
              | InvalidLetCaseNumCases Int
              | TypeNotQubits QType
              | TypeNotMeasuredQubits QType
              | UnificationMismatch [QType] [QType]
              | InvalidOperatorSizes
              | VariableAlreadyInScope T.Text
              | VariablesUsedMoreThanOnce (S.Set T.Text)
              | UnknownGate T.Text
              | IdentityGateIsNotIntegerSize T.Text Double
              | GateReceivedWrongNumberOfArguments T.Text Int Int
              | MatrixIsNotSquare [[Complex Double]]
              | MatrixIsNotAPowerOfTwo [[Complex Double]]
              | MatrixHasZeroQubits [[Complex Double]]
              | MatrixExceedsMaxSize Int [[Complex Double]]
              | InvalidPair Int [[Complex Double]]
              deriving (Show,Eq)

type ExceptInfer = Except TypeError
