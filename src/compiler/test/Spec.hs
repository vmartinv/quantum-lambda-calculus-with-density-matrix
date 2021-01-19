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
      parseLambdaRho "x" @?= (Var "x")
  , testCase "Parsing gates" $
      parseLambdaRho "U x" @?= (Gate "U" (Var "x"))
  , testCase "Parsing projector" $
      parseLambdaRho "PI x" @?= (Projector (Var "x"))
  , testCase "Parsing single qubit" $
      parseLambdaRho "|0>" @?= (Qubits "0")
  , testCase "Parsing multiple qubits" $
      parseLambdaRho "|01+->" @?= (Qubits "01+-")
  , testCase "Parsing otimes" $
      parseLambdaRho "x * y" @?= (Times (Var "x") (Var "y"))
  , testCase "Parsing lambda" $
      parseLambdaRho "\\x.y" @?= (Lambda "x" (Var "y"))
  , testCase "Parsing parenthesis" $
      parseLambdaRho "(\\x.x)" @?= (Lambda "x" (Var "x"))
  , testCase "Parsing function application" $
      parseLambdaRho "(\\x.x) y" @?= (FunApp (Lambda "x" (Var "x")) (Var "y"))
  , testCase "Parsing letcase" $
      parseLambdaRho "letcase x=PI y in {x,y}" @?= (LetCase "x" (Projector (Var "y")) [(Var "x"), (Var "y")])
  ]
