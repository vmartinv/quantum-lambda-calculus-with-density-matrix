module Typing.TypeCheckTests(typeCheckTests) where
import           Control.Monad.Except
import           Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Typing.QType
import           Typing.TypeChecker

testExp = runExcept.typeCheck
testStr = runExcept.(typeCheck<=<parseLambdaRho)

typeCheckTests :: TestTree
typeCheckTests = testGroup "Type Checker tests"
  [ testCase "single qubit" $
      testExp (PQubits "0") @?= Right (QTQubits 1)
  , testCase "multiple qubits" $
      testExp (PQubits "01+-") @?= Right (QTQubits 4)
  , testCase "identity" $
      testExp (PLambda "x" (PVar "x")) @?= Right (QTFun (QTQubits 1) (QTQubits 1))
  , testCase "lambda with gate" $
      testExp (PLambda "x" (PGate "U" (PVar "x"))) @?= Right (QTFun (QTQubits 2) (QTQubits 2))
  , testCase "lambda with projector" $
      testExp (PLambda "x" (PProjector (PVar "x"))) @?= Right (QTFun (QTQubits 1) (QTMeasuredQubits 1))
  , testCase "otimes" $
      testExp (PTimes (PQubits "01+-") (PQubits "0")) @?= Right (QTQubits 5)
  , testCase "Parsing function application" $
      testExp (PFunApp (PLambda "x" (PVar "x")) (PQubits "0")) @?= Right (QTQubits 1)
  , testCase "Solve EQ" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi y in {U (x * y), U (y * z), U (x * z), U (x * z)}" @?=
        Left "UnboundVariable \"x\""
  , testCase "Solve invalid EQ" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi y in {U (x * y), U (y * z), x, z}" @?=
        Left "UnboundVariable \"x\""
  , testCase "Unbound variable" $
      testStr "x" @?=
        Left "UnboundVariable \"x\""
  , testCase "Invalid num cases" $
      testStr "\\x.letcase xm=\\pi x in {xm, xm, xm}" @?=
        Left "InvalidLetCaseNumCases 3"
  , testCase "Invalid num cases 2" $
      testStr "letcase xm=\\pi |+> in {|0>, |0>, |0>, |0>}" @?=
        Left "InvalidOperatorSizes"
  , testCase "No-cloning theorem" $
      testStr "\\x.x*x" @?=
         Left "VariablesUsedMoreThanOnce (fromList [\"x\"])"
  ]
