module Translation.TranslationTests(translationTests) where
import           Data.Text               as T
import           Parsing.LamRhoExp
import           Python.PyExp
import           Python.PyRender
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck   as QC
import           Test.Tasty.SmallCheck   as SC
import           Translation.Translation

translateStr = T.replace "    " "" . T.replace "\n" "" . T.pack . pyRenderStr . translate

translationTests = testGroup "translationTests"
  [ testCase "qubits" $
    translateStr (PQubits "0") @?= "Circuit(1)"
  , testCase "projector" $
    translateStr (PProjector 1 (PQubits "0")) @?= "Circuit(1).measure(0)"
  , testCase "lambda and projector" $
    translateStr (PLambda "x" (PProjector 1 (PVar "x"))) @?= "lambda x: x.measure(0)"
  , testCase "Letcase" $
    translateStr (PLetCase "y" (PProjector 1 (PQubits "1")) [PQubits "1",PQubits "0"]) @?= "Circuit(1).measure(0,2)"
  , testCase "add one two qubits" $
    translateStr (PLambda "x" (PLetCase "y" (PProjector 1 (PVar "x")) [PQubits "1",PQubits "0"])) @?= "Circuit(1).measure(0,2)"
  ]
