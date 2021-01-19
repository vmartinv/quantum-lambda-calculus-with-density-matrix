import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Data.List
import           Data.Ord

import           Grammar

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lexer/Parser tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  []

qcProps = testGroup "(checked by QuickCheck)"
  []

unitTests = testGroup "Unit tests"
  [ testCase "Parsing var" $
      parseLambdaRho "x" @?= (PVar "x")
  , testCase "Parsing gates" $
      parseLambdaRho "U x" @?= (PGate "U" (PVar "x"))
  , testCase "Parsing projector" $
      parseLambdaRho "PI x" @?= (PProjector (PVar "x"))
  , testCase "Parsing single qubit" $
      parseLambdaRho "|0>" @?= (PQubits "0")
  , testCase "Parsing multiple qubits" $
      parseLambdaRho "|01+->" @?= (PQubits "01+-")
  , testCase "Parsing otimes" $
      parseLambdaRho "x * y" @?= (PTimes (PVar "x") (PVar "y"))
  , testCase "Parsing lambda" $
      parseLambdaRho "\\x.y" @?= (PLambda "x" (PVar "y"))
  , testCase "Parsing parenthesis" $
      parseLambdaRho "(\\x.x)" @?= (PLambda "x" (PVar "x"))
  , testCase "Parsing function application" $
      parseLambdaRho "(\\x.x) y" @?= (PFunApp (PLambda "x" (PVar "x")) (PVar "y"))
  , testCase "Parsing letcase" $
      parseLambdaRho "letcase x=PI y in {x,y}" @?= (PLetCase "x" (PProjector (PVar "y")) [(PVar "x"), (PVar "y")])
  ]
