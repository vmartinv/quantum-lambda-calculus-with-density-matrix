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
import           Translation.Translation

translateStr :: LamRhoExp -> T.Text
translateStr = removeWhiteSpaces . T.pack . pyRenderStr . translate

translationTests = testGroup "translationTests"
  [ testCase "qubits" $
    translateStr (PQubits "0") @?= "Circuit([1.0, 0.0, 0.0, 0.0,])"
  , testCase "qubits1" $
    translateStr (PQubits "1") @?= "Circuit([0.0, 0.0, 0.0, 1.0,])"
  , testCase "projector" $
    translateStr (PProjector 1 (PQubits "0")) @?= qubit0Str<>".measure(0)"
  , testCase "lambda and projector" $
    translateStr (PLambda "x" (PProjector 1 (PVar "x"))) @?= "lambda x: x.measure(0)"
  , testCase "Letcase" $
    translateStr (PLetCase "y" (PProjector 1 (PQubits "1")) [PQubits "1",PQubits "0"]) @?= "letcase("<>qubit1Str<>".measure(0),[lambda: "<>qubit1Str<>",lambda: "<>qubit0Str<>",])"
  , testCase "add one two qubits" $
    translateStr (PLambda "x" (PLetCase "y" (PProjector 1 (PVar "x")) [PQubits "1",PQubits "0"])) @?= "lambda x: letcase(x.measure(0),[lambda: "<>qubit1Str<>",lambda: "<>qubit0Str<>",])"
  ]
  where
      qubit0Str = translateStr (PQubits "0")
      qubit1Str = translateStr (PQubits "1")
