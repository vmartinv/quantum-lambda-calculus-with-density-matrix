module Typing.TypeCheckTests(typeCheckTests) where
import           Control.Monad.Except
import           Parsing.LamRhoExp
import           Parsing.LamRhoParser
import           Test.Tasty
import           Test.Tasty.HUnit
import CompilerError
import Typing.Hindley
import           Typing.QType
import           Typing.TypeEq
import           Typing.TypeChecker
import           Test.Tasty.QuickCheck      as QC
import           Test.Tasty.SmallCheck      as SC
import qualified Data.Map             as M

testExp = runExcept.(withExcept show).typeCheck
testStr = runExcept.(withExcept show).(typeCheck<=<parseLambdaRho)

typeCheckTests = testGroup "typeCheckTests" [qubitTests, lambdaAppTests, projectorTests, noCloningTests, gateParamsTests, letcaseTests, otimesTests, eqSolvingTests, matrixTests]

qubitTests = testGroup "qubitTests"
  [ testCase "Single qubit" $
      testExp (PQubits "0") @?= Right (QTQubits 1)
  , testCase "Multiple qubits" $
      testExp (PQubits "01+-") @?= Right (QTQubits 4)
  ]

lambdaAppTests = testGroup "lambdaAppTests"
  [ testCase "Identity" $
      testExp (PLambda "x" (PVar "x")) @?= Right (QTFun (QTQubits 1) (QTQubits 1))
  , testCase "Parsing function application" $
      testExp (PFunApp (PLambda "x" (PVar "x")) (PQubits "0")) @?= Right (QTQubits 1)
  , testCase "Single unbound variable" $
      testStr "x" @?=
        Left "UnboundVariable \"x\""
  , testCase "Already in scope error" $
     testStr "\\x.\\x.x" @?=
        Left "VariableAlreadyInScope \"x\""
  , testCase "Invalid app" $
      testStr "(\\x.x \\ket{0}) \\ket{0}" @?=
        Left "UnificationFail $(1)$ $(1) -> V0$"
  ]

projectorTests = testGroup "projectorTests"
  [ testCase "Double projector" $
    testStr "\\pi^1 \\pi^1 \\ket{0}" @?=
      Left "TypeNotQubits $(1, 1)$"
  , testCase "Lambda with projector" $
      testExp (PLambda "x" (PProjector 1 (PVar "x"))) @?= Right (QTFun (QTQubits 1) (QTMeasuredQubits 1))
  , testCase "qubit with too big projector" $
      testExp (PProjector 2 (PQubits "0")) @?= Left "InvalidOperatorSizes"
  ]

noCloningTests = testGroup "noCloningTests"
  [ testCase "No-cloning theorem" $
    testStr "\\x.x \\otimes x" @?=
      Left "VariablesUsedMoreThanOnce (fromList [\"x\"])"
  , testCase "No-cloning theorem letcase" $
    testStr "\\x. (letcase xm=\\pi^1 x in {\\ket{0}, \\ket{1}}) \\otimes (letcase xm=\\pi^1 x in {\\ket{0}, \\ket{1}}) " @?=
      Left "VariablesUsedMoreThanOnce (fromList [\"x\"])"
  ]

gateParamsTests = testGroup "gateParamsTests"
  [ testCase "Lambda with gate" $
      testExp (PLambda "x" (PGateApp (PGate "SWAP" [] 0) (PVar "x"))) @?= Right (QTFun (QTQubits 2) (QTQubits 2))
  , testCase "Lambda with gate with arguments" $
      testExp (PLambda "x" (PGateApp (PGate "CU" [1,2,3,4] 0) (PVar "x"))) @?= Right (QTFun (QTQubits 2) (QTQubits 2))
  , testCase "Lambda with gate with too many arguments" $
      testExp (PLambda "x" (PGateApp (PGate "CU" [1,2,3,4,5] 0) (PVar "x"))) @?= Left "GateReceivedWrongNumberOfArguments \"CU\" 4 5"
  , testCase "Lambda with SWAP with too many arguments" $
      testExp (PLambda "x" (PGateApp (PGate "SWAP" [1] 0) (PVar "x"))) @?= Left "GateReceivedWrongNumberOfArguments \"SWAP\" 0 1"
  , testCase "Lambda with CCNOT" $
      testExp (PLambda "x" (PGateApp (PGate "CCNOT" [] 0) (PVar "x"))) @?= Right (QTFun (QTQubits 3) (QTQubits 3))
  , testCase "Lambda with CSWAP" $
      testExp (PLambda "x" (PGateApp (PGate "CSWAP" [] 0) (PVar "x"))) @?= Right (QTFun (QTQubits 3) (QTQubits 3))
  , testCase "Lambda with CCNOT with too many arguments" $
      testExp (PLambda "x" (PGateApp (PGate "CCNOT" [1] 0) (PVar "x"))) @?= Left "GateReceivedWrongNumberOfArguments \"CCNOT\" 0 1"
  , testCase "Lambda with CSWAP with too many arguments" $
      testExp (PLambda "x" (PGateApp (PGate "CSWAP" [1] 0) (PVar "x"))) @?= Left "GateReceivedWrongNumberOfArguments \"CSWAP\" 0 1"
  , testCase "Lambda with gate with too few arguments" $
      testExp (PLambda "x" (PGateApp (PGate "CU" [1,2,3] 0) (PVar "x"))) @?= Left "GateReceivedWrongNumberOfArguments \"CU\" 4 3"
  , testCase "Lambda with U gate with too many arguments" $
      testExp (PLambda "x" (PGateApp (PGate "U" [1,2,3,5] 0) (PVar "x"))) @?= Left "GateReceivedWrongNumberOfArguments \"U\" 3 4"
  , testCase "Lambda with unknown gate" $
      testExp (PLambda "x" (PGateApp (PGate "WEIRD" [1,2] 0) (PVar "x"))) @?= Left "UnknownGate \"WEIRD\""
  , testCase "Lambda with identity of negative size" $
      testExp (PLambda "x" (PGateApp (PGate "I" [-1] 0) (PVar "x"))) @?= Left "IdentityGateIsNotIntegerSize \"I\" (-1.0)"
  , testCase "Lambda with identity of float size" $
      testExp (PLambda "x" (PGateApp (PGate "I" [1.2] 0) (PVar "x"))) @?= Left "IdentityGateIsNotIntegerSize \"I\" 1.2"
  , testCase "Lambda with identity of zero size" $
      testExp (PLambda "x" (PGateApp (PGate "I" [0] 0) (PVar "x"))) @?= Right (QTFun (QTQubits 1) (QTQubits 1))
  , testCase "Lambda with identity of two size" $
      testExp (PLambda "x" (PGateApp (PGate "I" [0,1] 0) (PVar "x"))) @?= Left "GateReceivedWrongNumberOfArguments \"I\" 1 2"
  , testCase "Lambda with identity with gate position" $
      testExp (PLambda "x" (PGateApp (PGate "I" [2] 2) (PVar "x"))) @?= Right (QTFun (QTQubits 4) (QTQubits 4))
  , testCase "Lambda with U with gate position" $
      testExp (PLambda "x" (PGateApp (PGate "U" [2,2,3] 2) (PVar "x"))) @?= Right (QTFun (QTQubits 3) (QTQubits 3))
  , testCase "Lambda with gate and position" $
      testExp (PLambda "x" (PGateApp (PGate "CU" [1,2,3,4] 4) (PVar "x"))) @?= Right (QTFun (QTQubits 6) (QTQubits 6))
  ]

letcaseTests = testGroup "letcaseTests"
  [ testCase "letcase not measured" $
     testStr "letcase ym=\\ket{+} in {\\ket{1}, \\ket{0}}" @?=
        Left "UnificationFail $(1)$ $(1, 1)$"
  , testCase "letcase ok" $
      testStr "letcase ym=\\pi^1 \\ket{+} in {\\ket{1}, \\ket{0}}" @?=
        Right (QTQubits 1)
  , testCase "Invalid num cases" $
      testStr "\\x.letcase xm=\\pi^2 x in {xm, xm, xm}" @?=
        Left "InvalidLetCaseNumCases 3"
  , testCase "Invalid num cases 0" $
      testExp (PLambda "x" (PLetCase "xm" (PProjector 2 (PVar "x")) [])) @?=
        Left "InvalidLetCaseNumCases 0"
  , testCase "Invalid num cases 0 intern" $
      runExcept (runHindleyM (TypeEnv M.empty) (equalTypes [])) @?=
        Left (InvalidLetCaseNumCases 0)
  , testCase "Invalid num cases 2" $
      testStr "letcase xm=\\pi^1 \\ket{+} in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}" @?=
        Left "UnificationFail $(1, 1)$ $(2, 2)$"
  , testCase "letcase with var" $
      testStr "\\x.letcase xm=x in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}" @?=
        Right (QTFun (QTMeasuredQubits 2) (QTQubits 1))
  , testCase "letcase with var applied" $
      testStr "(\\x.letcase xm=x in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}) (\\pi^2 \\ket{101})" @?=
        Right (QTQubits 1)
  , testCase "letcase with unused qubits" $
      testStr "letcase ym=\\pi^1 \\ket{++} in {\\ket{1}, \\ket{0}}" @?=
        Right (QTQubits 1)
  , testCase "letcase returning functions" $
     testStr "letcase ym=\\pi^1 \\ket{+} in {\\x.\\y.x, \\x.\\y.y}" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 1)))
  , testCase "Use external scope in cases" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi^2 y in {U (x \\otimes y), U (y \\otimes z), U (x \\otimes z), U (x \\otimes z)}" @?=
        Left "UnboundVariable \"x\""
  , testCase "Use external scope in cases 2" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi^2 y in {U (x \\otimes y), U (y \\otimes z), x, z}" @?=
        Left "UnboundVariable \"x\""
  , testCase "Use external scope in cases 3" $
      testStr "\\x.\\y. letcase ym=\\pi^2 x in {y, \\ket{000}}" @?=
        Left "UnboundVariable \"y\""
  ]

otimesTests = testGroup "otimesTests"
  [ testCase "Otimes" $
      testExp (POtimesExp (PQubits "01+-") (PQubits "0")) @?= Right (QTQubits 5)
  , testCase "Otimes with measured" $
      testStr "\\ket{+} \\otimes \\pi^1 \\ket{0}" @?=
        Left "TypeNotQubits $(1, 1)$"
  , testCase "Otimes by fun" $
     testStr "\\ket{+} \\otimes (\\x.x)" @?=
        Left "TypeNotQubits $V1 -> V1$"
  , testCase "Lambda with otimes" $
     testStr "\\x.\\y. x\\otimes y" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 2)))
  ]

eqSolvingTests = testGroup "eqSolvingTests"
  [ testCase "Big projection over unknowns with otimes" $
      testStr "\\x.\\y. \\pi^10 (x \\otimes y)" @?=
        Right (QTFun (QTQubits 9) (QTFun (QTQubits 1) (QTMeasuredQubits 10)))
  , testCase "show equation" $
      show (AtLeastSizeEq [QTQubits 9] (QTQubits 10)) @?=
        "AtLeastSizeEq [$(9)$] $(10)$"
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
      testStr "\\x.\\y.\\z. letcase ym=\\pi^3 (x \\otimes y \\otimes z) in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 1))))
  , testCase "Projection on 2 unknowns with letcase" $
      testStr "\\x.\\y. letcase ym=\\pi^3 (x \\otimes y) in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}" @?=
        Right (QTFun (QTQubits 2) (QTFun (QTQubits 1) (QTQubits 1)))
  , testCase "Nested letcase" $
      testStr "\\x.\\y. letcase ym=\\pi^1 (letcase zz=\\pi^3 (x \\otimes y) in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}) in {\\ket{1}, \\ket{+}}" @?=
        Right (QTFun (QTQubits 2) (QTFun (QTQubits 1) (QTQubits 1)))
  ]

matrixTests = testGroup "matrixTests"
  [ testCase "Zero qubit matrix" $
      testExp (PMatrix [[1]]) @?= Left "MatrixHasZeroQubits [[1.0 :+ 0.0]]"
  , testCase "Single qubit matrix" $
      testExp (PMatrix [[1,2],[3,4]]) @?= Right (QTQubits 1)
  , testCase "Triple qubit matrix" $
      testExp (PMatrix [[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8],[1,2,3,4,5,6,7,8]]) @?= Right (QTQubits 3)
  , QC.testProperty "big numbers matrix" $
      (\x -> testExp (PMatrix (replicate 8 (replicate 8 x))) /= Right (QTQubits 8)) . QC.getNonZero
  , testCase "Non square row matrix" $
      testExp (PMatrix [[1,2,3]]) @?= Left "MatrixIsNotSquare [[1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]]"
  , testCase "Non square column matrix" $
      testExp (PMatrix [[1],[1],[1]]) @?= Left "MatrixIsNotSquare [[1.0 :+ 0.0],[1.0 :+ 0.0],[1.0 :+ 0.0]]"
  , testCase "Different row/column size matrix" $
      testExp (PMatrix [[1],[1,2],[1]]) @?= Left "MatrixIsNotSquare [[1.0 :+ 0.0],[1.0 :+ 0.0,2.0 :+ 0.0],[1.0 :+ 0.0]]"
  , testCase "Matrix not power of 2" $
      testExp (PMatrix [[1,2,3],[1,2,3],[1,2,3]]) @?= Left "MatrixIsNotAPowerOfTwo [[1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0],[1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0],[1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]]"
  , testCase "Pair with qubit matrix" $
      testExp (PPair 1 [[1,2],[3,4]]) @?= Right (QTMeasuredQubits 1)
  , testCase "Pair with qubit matrix with invalid result" $
      testExp (PPair 2 [[1,2],[3,4]]) @?= Left "InvalidPair 2 [[1.0 :+ 0.0,2.0 :+ 0.0],[3.0 :+ 0.0,4.0 :+ 0.0]]"
  ]
