module Typing.GateCheck where

import           Control.Monad.Except
import qualified Data.Text            as T
import           Parsing.PExp
import           Typing.TypeError

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

getGateSizeNoCheck :: PGate -> Int
getGateSizeNoCheck g = either (const 0) id (runExcept (getGateSize g))

getGateSize :: PGate -> ExceptInfer Int
getGateSize (PGateOtimes g1 g2) = liftM2 (+) (getGateSize g1) (getGateSize g2)
getGateSize (PGate name@"U" params) = assertParamSize name 3 params *> return 1
getGateSize (PGate name@"UC" params) = assertParamSize name 3 params *> return 2
getGateSize (PGate name@"SWAP" params) = assertParamSize name 0 params *> return 2
getGateSize (PGate name@"CCNOT" params) = assertParamSize name 0 params *> return 3
getGateSize (PGate name@"CSWAP" params) = assertParamSize name 0 params *> return 3
getGateSize (PGate name@"I" params) = do
  assertParamSize name 1 params
  when (not (isInt nf)) (throwError $ IdentityGateIsNotIntegerSize nf)
  return n
  where
    nf = head params
    n = round nf
getGateSize (PGate name _params) = throwError $ UnknownGate name

assertParamSize :: T.Text -> Int -> [a] -> ExceptInfer ()
assertParamSize gate n ls =
  when (n/=length ls) (throwError $ GateReceivedWrongNumberOfArguments n (length ls))
