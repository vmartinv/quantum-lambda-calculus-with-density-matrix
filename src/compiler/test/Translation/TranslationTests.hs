module Translation.TranslationTests(translationTests) where
import           Data.Text               as T
import           Parsing.LamRhoExp
import           Python.PyExp
import           Python.PyRender
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck   as QC
import           Test.Tasty.SmallCheck   as SC
import           TestUtils
import           Data.Complex
import           Translation.Translation

translateStr :: LamRhoExp -> T.Text
translateStr = removeWhiteSpaces . T.pack . pyRenderStr . translate

translationTests = testGroup "translationTests"
  [ testCase "qubits" $
    translateStr (PQubits "0") @?= "Circuit([1.0, 0.0, 0.0, 0.0,])"
  , testCase "qubits1notstr" $
    translate (PQubits "1") @?= PyFunCall (PyFun "Circuit") [PyList [PyComplex (0.0 :+ 0.0),PyComplex (0.0 :+ 0.0),PyComplex (0.0 :+ 0.0),PyComplex (1.0 :+ 0.0)]]
  , testCase "qubits1show" $
    (show . translate) (PQubits "1") @?= "PyFunCall (PyFun \"Circuit\") [PyList [PyComplex (0.0 :+ 0.0),PyComplex (0.0 :+ 0.0),PyComplex (0.0 :+ 0.0),PyComplex (1.0 :+ 0.0)]]"
  , testCase "qubits1" $
    translateStr (PQubits "1") @?= "Circuit([0.0, 0.0, 0.0, 1.0,])"
  , testCase "qubits+-" $
    translateStr (PQubits "+-") @?= "Circuit([0.0 + 7.354136628764637e-9j,0.0 + 2.45137887625488e-9j,0.49999999999999994,-0.4999999999999998,0.0 - 2.45137887625488e-9j,0.0 + 2.45137887625488e-9j,0.4999999999999998,-0.4999999999999998,0.0,0.0 + 9.740904475271228e-17j,0.0,0.0,0.0 + 4.87045223763561e-17j,0.0 - 4.870452237635613e-17j,0.0,0.0,])"
  , testCase "matrix" $
    translateStr (PMatrix [[0:+1, 1:+(-1)],[(-3.1415):+0,0.123:+ (99999e3)]]) @?= "Circuit([0.7071067589725651 + 0.7071067811865474j,2.22139819202576e-8 - 2.221398116775141e-8j,8.697547077083506e-14 - 1.4142206476362872e-4j,7071.032460786757 + 7071.032452089297j,])"
  , testCase "projector" $
    translateStr (PProjector 1 (PQubits "0")) @?= qubit0Str<>".measure(0)"
  , testCase "show fun name" $
    pyRenderStr (PyFun "asdf") @?= "asdf"
  , testCase "lambda and projector" $
    translateStr (PLambda "x" (PProjector 1 (PVar "x"))) @?= "lambda x: x.measure(0)"
  , testCase "gate" $
    translateStr (PGateApp (PGate "CU" [1,2,3] 8) (PVar "x")) @?= "x.cu(1.0,2.0,3.0)"
  , testCase "Letcase" $
    translateStr (PLetCase "y" (PProjector 1 (PQubits "1")) [PQubits "1",PQubits "0"]) @?= "letcase("<>qubit1Str<>".measure(0),[lambda: "<>qubit1Str<>",lambda: "<>qubit0Str<>",])"
  , testCase "add one two qubits" $
    translateStr (PLambda "x" (PLetCase "y" (PProjector 1 (PVar "x")) [PQubits "1",PQubits "0"])) @?= "lambda x: letcase(x.measure(0),[lambda: "<>qubit1Str<>",lambda: "<>qubit0Str<>",])"
  ]
  where
      qubit0Str = translateStr (PQubits "0")
      qubit1Str = translateStr (PQubits "1")
