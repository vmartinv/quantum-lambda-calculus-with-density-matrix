# quantum-lambda-calculus-with-density-matrix
Transpiler in Haskell for a Lambda Calculus for Density Matrices with Classical and Probabilistic Controls


Requires stack


stack ghci compiler:compiler-test
:set prompt "Haskell> "
:l Translation.StateBuilderTests
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM

st = [0,1,0,0]:: [Complex Double]
stateToZeroGates st
applyGatesV (stateToZeroGates st) (HM.fromList st)
