module Typing.GateCheck where

import           Control.Monad.Except
import qualified Data.Text            as T
import           Typing.PExp
import           Typing.TypeError

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

getGateSize :: [PGateDef] -> ExceptInfer Int
getGateSize defs = sum <$> sequence (getGateDefSize <$> defs)

getGateDefSizeNoCheck :: PGateDef -> Int
getGateDefSizeNoCheck gdef = either (const 0) id (runExcept (getGateDefSize gdef))

getGateDefSize :: PGateDef -> ExceptInfer Int
getGateDefSize (PGateDef name@"U" params) = assertParamSize name 3 params *> return 1
getGateDefSize (PGateDef name@"UC" params) = assertParamSize name 3 params *> return 2
getGateDefSize (PGateDef name@"SWAP" params) = assertParamSize name 0 params *> return 2
getGateDefSize (PGateDef name@"CCNOT" params) = assertParamSize name 0 params *> return 3
getGateDefSize (PGateDef name@"CSWAP" params) = assertParamSize name 0 params *> return 3
getGateDefSize (PGateDef name@"I" params) = do
  assertParamSize name 1 params
  when (not (isInt nf)) (throwError $ IdentityGateIsNotIntegerSize nf)
  return n
  where
    nf = head params
    n = round nf
getGateDefSize (PGateDef name _params) = throwError $ UnknownGate name

assertParamSize :: T.Text -> Int -> [a] -> ExceptInfer ()
assertParamSize gate n ls =
  when (n/=length ls) (throwError $ GateReceivedWrongNumberOfArguments n (length ls))
