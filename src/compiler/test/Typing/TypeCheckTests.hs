module Typing.TypeCheckTests(typeCheckTests) where
import           Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Typing.QType
import           Typing.TypeChecker

typeCheckTests :: TestTree
typeCheckTests = testGroup "Type Checker tests"
  [ testCase "single qubit" $
      typeCheck (PQubits "0") @?= Right (QTQubits 1)
  , testCase "multiple qubits" $
      typeCheck (PQubits "01+-") @?= Right (QTQubits 4)
  , testCase "identity" $
      typeCheck (PLambda "x" (PVar "x")) @?= Right (QTFun (QTVar 0) (QTVar 0))
  , testCase "lambda with gate" $
      typeCheck (PLambda "x" (PGate "U" (PVar "x"))) @?= Right (QTFun (QTVar 0) (QTVar 0))
  , testCase "lambda with projector" $
      typeCheck (PLambda "x" (PProjector (PVar "x"))) @?= Right (QTFun (QTVar 0) (QTVar 1))
  , testCase "otimes" $
      typeCheck (PTimes (PQubits "01+-") (PQubits "0")) @?= Right (QTQubits 5)
  , testCase "Parsing function application" $
      typeCheck (PFunApp (PLambda "x" (PVar "x")) (PQubits "0")) @?= Right (QTQubits 1)
  ]
