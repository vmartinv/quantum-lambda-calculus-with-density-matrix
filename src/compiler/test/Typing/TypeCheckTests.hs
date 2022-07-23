module Typing.TypeCheckTests(typeCheckTests) where
import           Control.Monad.Except
import           Parsing.Parser
import           Parsing.PExp
import           Test.Tasty
import           Test.Tasty.HUnit
import           Typing.QType
import           Typing.TypeChecker

testExp = runExcept.typeCheck
testStr = runExcept.(typeCheck<=<parseLambdaRho)

typeCheckTests :: TestTree
typeCheckTests = testGroup "Type Checker tests"
  [ testCase "Single qubit" $
      testExp (PQubits "0") @?= Right (QTQubits 1)
  , testCase "Multiple qubits" $
      testExp (PQubits "01+-") @?= Right (QTQubits 4)
  , testCase "Identity" $
      testExp (PLambda "x" (PVar "x")) @?= Right (QTFun (QTQubits 1) (QTQubits 1))
  , testCase "Lambda with gate" $
      testExp (PLambda "x" (PGateApp (PGate "SWAP" []) (PVar "x"))) @?= Right (QTFun (QTQubits 2) (QTQubits 2))
  , testCase "Lambda with gate with arguments" $
      testExp (PLambda "x" (PGateApp (PGate "UC" [1,2,3]) (PVar "x"))) @?= Right (QTFun (QTQubits 2) (QTQubits 2))
  , testCase "Lambda with gate with otimes" $
      testExp (PLambda "x" (PGateApp (PGateOtimes (PGateOtimes (PGate "SWAP" []) (PGate "I" [5])) (PGate "UC" [1,2,3])) (PVar "x"))) @?= Right (QTFun (QTQubits 9) (QTQubits 9))
  , testCase "Lambda with projector" $
      testExp (PLambda "x" (PProjector 1 (PVar "x"))) @?= Right (QTFun (QTQubits 1) (QTMeasuredQubits 1))
  , testCase "Otimes" $
      testExp (POtimes (PQubits "01+-") (PQubits "0")) @?= Right (QTQubits 5)
  , testCase "Parsing function application" $
      testExp (PFunApp (PLambda "x" (PVar "x")) (PQubits "0")) @?= Right (QTQubits 1)
  , testCase "Use external scope in cases" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi^2 y in {U (x \\otimes y), U (y \\otimes z), U (x \\otimes z), U (x \\otimes z)}" @?=
        Left "UnboundVariable \"x\""
  , testCase "Use external scope in cases 2" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi^2 y in {U (x \\otimes y), U (y \\otimes z), x, z}" @?=
        Left "UnboundVariable \"x\""
  , testCase "Double projector" $
      testStr "\\pi^1 \\pi^1 |0>" @?=
        Left "TypeNotQubits $(1, 1)$"
  , testCase "Single unbound variable" $
      testStr "x" @?=
        Left "UnboundVariable \"x\""
  , testCase "Invalid num cases" $
      testStr "\\x.letcase xm=\\pi^2 x in {xm, xm, xm}" @?=
        Left "InvalidLetCaseNumCases 3"
  , testCase "Invalid num cases 2" $
      testStr "letcase xm=\\pi^1 |+> in {|0>, |0>, |0>, |0>}" @?=
        Left "UnificationFail $(2, 2)$ $(1, 1)$"
  , testCase "No-cloning theorem" $
      testStr "\\x.x \\otimes x" @?=
        Left "VariablesUsedMoreThanOnce (fromList [\"x\"])"
  , testCase "No-cloning theorem letcase" $
      testStr "\\x. (letcase xm=\\pi^1 x in {|0>, |1>}) \\otimes (letcase xm=\\pi^1 x in {|0>, |1>}) " @?=
        Left "VariablesUsedMoreThanOnce (fromList [\"x\"])"
  , testCase "Already in scope error" $
     testStr "\\x.\\x.x" @?=
        Left "VariableAlreadyInScope \"x\""
  , testCase "Lambda with otimes" $
     testStr "\\x.\\y. x\\otimes y" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 2)))
  , testCase "Invalid app" $
      testStr "(\\x.x |0>) |0>" @?=
        Left "UnificationFail $(1)$ $(1) -> V0$"
  , testCase "letcase not measured" $
     testStr "letcase ym=|+> in {|1>, |0>}" @?=
        Left "UnificationFail $(1)$ $(1, 1)$"
  , testCase "letcase ok" $
      testStr "letcase ym=\\pi^1 |+> in {|1>, |0>}" @?=
        Right (QTQubits 1)
  , testCase "letcase with unused qubits" $
      testStr "letcase ym=\\pi^1 |++> in {|1>, |0>}" @?=
        Right (QTQubits 1)
  , testCase "letcase returning functions" $
     testStr "letcase ym=\\pi^1 |+> in {\\x.\\y.x, \\x.\\y.y}" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 1)))
  , testCase "Otimes with measured" $
      testStr "|+> \\otimes \\pi^1 |0>" @?=
        Left "TypeNotQubits $(1, 1)$"
  , testCase "Otimes by fun" $
     testStr "|+> \\otimes (\\x.x)" @?=
        Left "TypeNotQubits $V1 -> V1$"
  , testCase "Big projection over unknowns with otimes" $
      testStr "\\x.\\y. \\pi^10 (x \\otimes y)" @?=
        Right (QTFun (QTQubits 9) (QTFun (QTQubits 1) (QTMeasuredQubits 10)))
  , testCase "Big projection over unknowns with two otimes" $
      testStr "\\x.\\y.\\z. \\pi^10 (x \\otimes y \\otimes z)" @?=
        Right (QTFun (QTQubits 8) (QTFun (QTQubits 1)  (QTFun (QTQubits 1) (QTMeasuredQubits 10))))
  , testCase "Big projection over unknowns with many otimes" $
      testStr "\\a.\\b.\\c.\\d.\\e.\\f.\\g.\\h.\\i.\\j. \\pi^100 (a \\otimes b \\otimes c \\otimes d \\otimes e \\otimes f \\otimes g \\otimes h \\otimes i \\otimes j)" @?=
        Right (QTFun (QTQubits 91) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTMeasuredQubits 100)))))))))))
  , testCase "Big projection over unknowns with many otimes different assoc" $
      testStr "\\a.\\b.\\c.\\d.\\e.\\f.\\g.\\h.\\i.\\x.\\y.\\z. \\pi^100 (x \\otimes (y \\otimes (z \\otimes a \\otimes b) \\otimes c \\otimes d) \\otimes e \\otimes (f \\otimes g)\\otimes h \\otimes i)" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 89) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTMeasuredQubits 100)))))))))))))
  , testCase "Projection on 3 unknowns with letcase" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi^3 (x \\otimes y \\otimes z) in {|0>, |0>, |0>, |0>, |0>, |0>, |0>, |0>}" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 1))))
  , testCase "Projection on 2 unknowns with letcase" $
      testStr "\\x.\\y. letcase ym=\\pi^3 (x \\otimes y) in {|0>, |0>, |0>, |0>, |0>, |0>, |0>, |0>}" @?=
        Right (QTFun (QTQubits 2) (QTFun (QTQubits 1) (QTQubits 1)))
  , testCase "Nested letcase" $
      testStr "\\x.\\y. letcase ym=\\pi^1 (letcase zz=\\pi^3 (x \\otimes y) in {|0>, |0>, |0>, |0>, |0>, |0>, |0>, |0>}) in {|1>, |+>}" @?=
        Right (QTFun (QTQubits 2) (QTFun (QTQubits 1) (QTQubits 1)))
  , testCase "Zero qubit matrix" $
      testExp (PMatrix [[1]]) @?= Left "MatrixHasZeroQubits [[1.0]]"
  , testCase "Single qubit matrix" $
      testExp (PMatrix [[1,2],[3,4]]) @?= Right (QTQubits 1)
  , testCase "Triple qubit matrix" $
      testExp (PMatrix [[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8]]) @?= Right (QTQubits 3)
  , testCase "Non square row matrix" $
      testExp (PMatrix [[1,2,3]]) @?= Left "MatrixIsNotSquare [[1.0,2.0,3.0]]"
  , testCase "Non square column matrix" $
      testExp (PMatrix [[1],[1],[1]]) @?= Left "MatrixIsNotSquare [[1.0],[1.0],[1.0]]"
  , testCase "Different row/column size matrix" $
      testExp (PMatrix [[1],[1,2],[1]]) @?= Left "MatrixIsNotSquare [[1.0],[1.0,2.0],[1.0]]"
  ]
