module TypeCheckTests(typeCheckTests) where
import           Parser
import           QType
import           Test.Tasty
import           Test.Tasty.HUnit
import           TypeChecker

typeCheckTests :: TestTree
typeCheckTests = testGroup "Type Checker tests"
  [ testCase "single qubit" $
      typeCheck (PQubits "0") @?= Right (Forall [] $ QTQubits 1)
  , testCase "multiple qubits" $
      typeCheck (PQubits "01+-") @?= Right (Forall [] $ QTQubits 4)
  , testCase "identity" $
      typeCheck (PLambda "x" (PVar "x")) @?= Right (Forall ["a"] $ QTFun (QTVar "a") (QTVar "a"))
  , testCase "lambda with gate" $
      typeCheck (PLambda "x" (PGate "U" (PVar "x"))) @?= Right (Forall [] $  QTFun (QTQubits 1) (QTQubits 1))
  , testCase "lambda with projector" $
      typeCheck (PLambda "x" (PProjector (PVar "x"))) @?= Right (Forall [] $ QTFun (QTQubits 1) (QTMeasuredQubits 1))
  , testCase "Parsing function application" $
      typeCheck (PFunApp (PLambda "x" (PVar "x")) (PQubits "0")) @?= Right (Forall [] $ QTQubits 1)
  ]
