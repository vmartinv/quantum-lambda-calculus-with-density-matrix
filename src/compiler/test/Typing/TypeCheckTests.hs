module Typing.TypeCheckTests(typeCheckTests) where
import           Control.Monad.Except
import           Parsing.LamRhoExp
import           Parsing.LamRhoParser
import           Test.Tasty
import           Test.Tasty.HUnit
import Data.Complex
import CompilerError
import Typing.Hindley
import           Typing.QType
import           Typing.TypeEq
import           Typing.TypeChecker
import Typing.Robinson
import           Test.Tasty.QuickCheck      as QC
import           Test.Tasty.SmallCheck      as SC
import qualified Data.Map             as M
import Numeric.LinearProgramming

testExp = runExcept.(withExcept show).typeCheck
testStr = runExcept.(withExcept show).(typeCheck<=<parseLambdaRho)

typeCheckTests = testGroup "typeCheckTests" [qubitTests, lambdaAppTests, projectorTests, noCloningTests, gateParamsTests, letcaseTests, otimesTests, eqSolvingTests, matrixTests, betterCoverageTests]

qubitTests = testGroup "qubitTests"
  [ testCase "Single qubit" $
      testExp (PQubits "0") @?= Right (QTQubits 1)
  , testCase "Multiple qubits" $
      testExp (PQubits "01+-") @?= Right (QTQubits 4)
  ]

betterCoverageTests = testGroup "betterCoverageTests"
  [ testCase "Bind var to itself" $ 
      (runExcept.(withExcept show) $ (5 `bind` (QTVar 5))) @?= Right (M.empty)
  , testCase "Bind infinite type" $ 
      (runExcept.(withExcept show) $ (5 `bind` (QTFun (QTQubits 1) (QTVar 5)))) @?= Left "InfiniteType 5 $(1) -> V5$" 
  , testCase "assertQubitOrVar var" $ 
      (runExcept.(withExcept show) $ (assertQubitOrVar (QTVar 4))) @?= Right ()
  , testCase "assertQubitOrVar qubits" $ 
      (runExcept.(withExcept show) $ (assertQubitOrVar (QTQubits 100))) @?= Right ()
  , testCase "getQubitSize not qubit" $ 
      getQubitSize (QTVar 3) @?= Nothing
  , testCase "verifyEq at least ok" $ 
      verifyEq (AtLeastSizeEq [QTQubits 1,QTQubits 2,QTQubits 10] 12) @?= Just ()
  , testCase "verifyEq sum ok" $ 
      verifyEq (SumSizeEq [QTQubits 1,QTQubits 2,QTQubits 10] (QTQubits 13)) @?= Just ()
  , testCase "verifyEq sum fail" $ 
      verifyEq (SumSizeEq [QTQubits 1,QTQubits 2,QTQubits 10] (QTQubits 12)) @?= Nothing
  , testCase "verifyEq not eq" $ 
      verifyEq (EqualTypeEq (QTQubits 1) (QTQubits 2)) @?= Just ()
  , testCase "solveLinear notInteger sol" $
      (runExcept.(withExcept show) $ solveLinear 1 notIntSolution []) @?= Left "InvalidOperatorSizesNotIntegerSolution 1.5 []" 
  ]
  where
      notIntSolution = Sparse [[(2#1)] :==: 3.0]

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
  , testCase "zero projector" $
    testExp (PProjector 0 (PQubits "0")) @?= Left "InvalidProjectorSize 0"
  , testCase "Lambda with projector" $
      testExp (PLambda "x" (PProjector 1 (PVar "x"))) @?= Right (QTFun (QTQubits 1) (QTMeasuredQubits 1 (QTQubits 1)))
  , testCase "qubit.with.too.big.projector" $
      testExp (PProjector 2 (PQubits "0")) @?= Left "InvalidOperatorSizesCheckFailed [AtLeastSizeEq [$(1)$] 2] (fromList [])"
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
        Left "UnificationFail $(1)$ $(1, V0)$"
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
        Left "UnificationFail $(1, 1)$ $(2, V0)$"
  , testCase "letcase.with.var" $
      testStr "\\x.letcase xm=x in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}" @?=
        Right (QTFun (QTMeasuredQubits 2 (QTQubits 2)) (QTQubits 1))
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
  , testCase "Lambda.otimes.projector" $
     testStr "\\x.\\y. \\pi^3 (x\\otimes y)" @?=
        Right (QTFun (QTQubits 2) (QTFun (QTQubits 1) (QTMeasuredQubits 3 (QTQubits 3))))
  ]

eqSolvingTests = testGroup "eqSolvingTests"
  [ testCase "Big projection over unknowns with otimes" $
      testStr "\\x.\\y. \\pi^10 (x \\otimes y)" @?=
        Right (QTFun (QTQubits 9) (QTFun (QTQubits 1) (QTMeasuredQubits 10 (QTQubits 10))))
  , testCase "show equation" $
      show (AtLeastSizeEq [QTQubits 9] 10) @?=
        "AtLeastSizeEq [$(9)$] 10"
  , testCase "eq equation" $
      (AtLeastSizeEq [QTQubits 3] 8) @?=
        (AtLeastSizeEq [QTQubits 3] 8)
  , testCase "Big projection over unknowns with two otimes" $
      testStr "\\x.\\y.\\z. \\pi^10 (x \\otimes y \\otimes z)" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1)  (QTFun (QTQubits 8) (QTMeasuredQubits 10 (QTQubits 10)))))
  , testCase "Big projection over unknowns with many otimes" $
      testStr "\\a.\\b.\\c.\\d.\\e.\\f.\\g.\\h.\\i.\\j. \\pi^100 (a \\otimes b \\otimes c \\otimes d \\otimes e \\otimes f \\otimes g \\otimes h \\otimes i \\otimes j)" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 91) (QTMeasuredQubits 100 (QTQubits 100))))))))))))
  , testCase "Big projection over unknowns with many otimes different assoc" $
      testStr "\\a.\\b.\\c.\\d.\\e.\\f.\\g.\\h.\\i.\\x.\\y.\\z. \\pi^100 (x \\otimes (y \\otimes (z \\otimes a \\otimes b) \\otimes c \\otimes d) \\otimes e \\otimes (f \\otimes g)\\otimes h \\otimes i)" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 89) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTMeasuredQubits 100 
          (QTQubits 100))))))))))))))
  , testCase "Projection on 3 unknowns with letcase" $
      testStr "\\x.\\y.\\z. letcase ym=\\pi^3 (x \\otimes y \\otimes z) in {\\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}, \\ket{0}}" @?=
        Right (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTFun (QTQubits 1) (QTQubits 1))))
  , testCase "Projection.unknown.letcase.applying.gate.small" $
      testStr "\\x. letcase y=\\pi^2 x in {SWAP_2 y, SWAP y, y, SWAP_1 y}" @?=
        Right (QTFun (QTQubits 4) (QTQubits 4))
  , testCase "Projection.unknown.letcase.applying.gate.lambda" $
      testStr "\\x. (letcase y=\\pi^2 x in {\\z.z \\otimes SWAP_2 y, \\z.y \\otimes SWAP z, \\z.y \\otimes z, \\z.z \\otimes SWAP_1 y}) \\ket{01}" @?=
        Right (QTFun (QTQubits 4) (QTQubits 6))
  , testCase "Projection.unknown.letcase.applying.gate.lambda.eq.ok" $
      testStr "\\x. (letcase y=\\pi^2 x in {\\z.z \\otimes SWAP_2 y, \\z.y \\otimes SWAP z, \\z.y \\otimes z, \\z.\\ket{101011}}) \\ket{01}" @?=
        Right (QTFun (QTQubits 4) (QTQubits 6))
  , testCase "Projection.unknown.letcase.applying.gate.lambda.eq.fail" $
      testStr "\\x. (letcase y=\\pi^2 x in {\\z.z \\otimes SWAP_2 y, \\z.y \\otimes SWAP z, \\z.y \\otimes z, \\z.\\ket{10101}}) \\ket{01}" @?=
        Left "InvalidOperatorSizesNoSolution [AtLeastSizeEq [$V2$] 2,AtLeastSizeEq [$V2$] 4,SumSizeEq [$(2)$,$V2$] $(5)$,AtLeastSizeEq [$(2)$] 2,SumSizeEq [$V2$,$(2)$] $(5)$,SumSizeEq [$V2$,$(2)$] $(5)$,EqualTypeEq $(2) -> (5)$ $(2) -> (5)$,EqualTypeEq $(2) -> (5)$ $(2) -> (5)$,EqualTypeEq $(2) -> (5)$ $(2) -> (5)$,EqualTypeEq $(2, V2)$ $(2, V2)$,AtLeastSizeEq [$V2$] 2,EqualTypeEq $(2) -> (5)$ $(2) -> (5)$]"
  , testCase "Projection.unknown.letcase.applying.gate" $
      testStr "\\x. letcase y=\\pi^2 x in {SWAP_11 y, SWAP_10 y, y, y}" @?=
        Right (QTFun (QTQubits 13) (QTQubits 13))
  , testCase "trying to use var inside letcase" $
      testStr "\\z.\\x. letcase y=\\pi^2 x in {z, \\ket{0}}" @?=
        Left "UnboundVariable \"z\""
  , testCase "sumeq fail" $
      testStr "letcase ym=\\pi^1 \\ket{+} in {\\ket{1}\\otimes\\ket{11}, \\ket{0}\\otimes\\ket{0}}" @?=
       Left "InvalidOperatorSizesNoSolution [AtLeastSizeEq [$(1)$] 1,SumSizeEq [$(1)$,$(2)$] $V2$,SumSizeEq [$(1)$,$(1)$] $V2$,EqualTypeEq $V2$ $V2$,EqualTypeEq $(1, 1)$ $(1, 1)$,AtLeastSizeEq [$(1)$] 1]"
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
  , testCase "Invalid matrix trace" $
      testExp (PMatrix [[1,0],[0,0.1]]) @?= Left "MatrixTraceNot1 [[1.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,0.1 :+ 0.0]] (1.1 :+ 0.0)"
  , testCase "Invalid matrix not positive" $
      testExp (PMatrix [[1,1],[0:+1,0]]) @?= Left "MatrixNotPositive [[1.0 :+ 0.0,1.0 :+ 0.0],[0.0 :+ 1.0,0.0 :+ 0.0]] [1.3002425902201207 :+ 0.624810533843827,(-0.30024259022012034) :+ (-0.6248105338438266)]"
  , testCase "Single qubit matrix" $
      testExp (PMatrix [[1,0],[0,0]]) @?= Right (QTQubits 1)
  , testCase "Triple qubit matrix" $
      testExp (PMatrix [[0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,1]]) @?= Right (QTQubits 3)
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
      testExp (PPair 1 1 [[0,0],[0,1]]) @?= Right (QTMeasuredQubits 1 (QTQubits 1))
  , testCase "Pair with qubit matrix with invalid result" $
      testExp (PPair 2 2 [[0,0],[0,1]]) @?= Left "InvalidPair 2 2 [[0.0 :+ 0.0,0.0 :+ 0.0],[0.0 :+ 0.0,1.0 :+ 0.0]]"
  ]
