module Typing.QTypeTests(qTypeTests) where
import           Test.Tasty
import           Test.Tasty.HUnit

import           Typing.QType

qTypeTests :: TestTree
qTypeTests = testGroup "QType tests"
  [ testCase "Show qubits" $
      show (QTQubits 5) @?= "$(5)$"
  , testCase "Show measured bits" $
      show (QTMeasuredQubits 4) @?= "$(4, 4)$"
  , testCase "Show simple fun" $
      show (QTFun (QTQubits 1) (QTMeasuredQubits 2)) @?= "$(1) \\multimap (2, 2)$"
  , testCase "Show high-order fun left" $
      show (QTFun (QTFun (QTQubits 1) (QTQubits 3)) (QTMeasuredQubits 2)) @?= "$((1) \\multimap (3)) \\multimap (2, 2)$"
  , testCase "Show high-order fun right" $
      show (QTFun (QTMeasuredQubits 2) (QTFun (QTQubits 1) (QTQubits 3))) @?= "$(2, 2) \\multimap ((1) \\multimap (3))$"
  ]
