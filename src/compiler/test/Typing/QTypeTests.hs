module Typing.QTypeTests(qTypeTests) where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Typing.QType

qTypeTests = testGroup "qTypeTests"
  [ testCase "Show qubits" $
      show (QTQubits 5) @?= "$(5)$"
  , testCase "Show measured bits" $
      show (QTMeasuredQubits 4 (QTQubits 1)) @?= "$(4, 1)$"
  , testCase "Show simple fun" $
      show (QTFun (QTQubits 1) (QTMeasuredQubits 2 (QTQubits 3))) @?= "$(1) -> (2, 3)$"
  , testCase "Show high-order fun left" $
      show (QTFun (QTFun (QTQubits 1) (QTQubits 3)) (QTMeasuredQubits 2 (QTQubits 5))) @?= "$((1) -> (3)) -> (2, 5)$"
  , testCase "Show high-order fun right" $
      show (QTFun (QTMeasuredQubits 2 (QTQubits 4)) (QTFun (QTQubits 1) (QTQubits 3))) @?= "$(2, 4) -> ((1) -> (3))$"
  ]
