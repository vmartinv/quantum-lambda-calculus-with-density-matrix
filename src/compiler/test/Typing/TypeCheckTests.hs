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
  , testCase "In scope error" $
     testStr "\\x.\\x.x" @?=
        Left "VariableAlreadyInScope \"x\""
  , testCase "Invalid app" $
      testStr "(\\x.x |0>) |0>" @?=
        Left "UnificationFail $(1)$ $(1) \\multimap V0$"
  , testCase "let not measured" $
     testStr "letcase ym=|+> in {|1>, |0>}" @?=
        Left "UnificationFail $(1)$ $(1, 1)$"
  , testCase "let ok" $
      testStr "letcase ym=\\pi |+> in {|1>, |0>}" @?=
        Right (QTQubits 1)
  , testCase "let ok fun" $
     testStr "letcase ym=\\pi |+> in {\\x.\\y.x, \\x.\\y.y}" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 1)))
  , testCase "times by measured" $
      testStr "|+> * \\pi |0>" @?=
        Left "TypeNotQubits $(1, 1)$"
  , testCase "times by fun" $
     testStr "|+> * (\\x.x)" @?=
        Left "TypeNotQubits $V1 \\multimap V1$"
  , testCase "Solve EQ" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi (x*y*z) in {|0>, |0>, |0>, |0>, |0>, |0>, |0>, |0>}" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 1))))
  , testCase "Solve EQ2" $
      testStr "\\x.\\y. letcase ym=\\pi (x*y) in {|0>, |0>, |0>, |0>, |0>, |0>, |0>, |0>}" @?=
        Right (QTFun (QTQubits 2) (QTFun (QTQubits 1) (QTQubits 1)))
  , testCase "Nested let" $
      testStr "\\x.\\y. letcase ym=\\pi (letcase zz=\\pi (x*y) in {|0>, |0>, |0>, |0>, |0>, |0>, |0>, |0>}) in {|1>, |+>}" @?=
        Right (QTFun (QTQubits 2) (QTFun (QTQubits 1) (QTQubits 1)))
  ]
