module Typing.GateChecker where

import           CompilerError
import           Control.Monad.Except
import qualified Data.Text            as T
import           Parsing.LamRhoExp

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

getGateSizeNoCheck :: PGate -> Int
getGateSizeNoCheck g = either (const 0) id (runExcept (getGateSize g))

getGateSize :: PGate -> ExceptInfer Int
getGateSize (PGate name@"U" params _) = assertParamSize name 3 params *> return 1
getGateSize (PGate name@"CU" params _) = assertParamSize name 4 params *> return 2
getGateSize (PGate name@"SWAP" params _) = assertParamSize name 0 params *> return 2
getGateSize (PGate name@"CCNOT" params _) = assertParamSize name 0 params *> return 3
getGateSize (PGate name@"CSWAP" params _) = assertParamSize name 0 params *> return 3
getGateSize (PGate name@"I" params _) = do
  assertParamSize name 1 params
  when (not (isInt nf)) (throwError $ IdentityGateIsNotIntegerSize name nf)
  when (n < 0) (throwError $ IdentityGateIsNotIntegerSize name nf)
  return n
  where
    nf = head params
    n = round nf
getGateSize (PGate name _params _) = throwError $ UnknownGate name

assertParamSize :: T.Text -> Int -> [a] -> ExceptInfer ()
assertParamSize gate n ls =
  when (n/=length ls) (throwError $ GateReceivedWrongNumberOfArguments gate n (length ls))
