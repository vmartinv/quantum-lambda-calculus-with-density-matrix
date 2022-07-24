module Typing.TypeError where

import           Control.Monad.Except
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
              | MatrixIsNotSquare [[Double]]
              | MatrixIsNotAPowerOfTwo [[Double]]
              | MatrixHasZeroQubits [[Double]]
              deriving (Show,Eq)

type ExceptInfer = Except TypeError
