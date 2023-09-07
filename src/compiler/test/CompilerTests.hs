module CompilerTests(compilerTests) where
import           Compiler
import           Data.Text               as T
import           Parsing.LamRhoExp
import           Python.PyRender
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck   as QC
import           Test.Tasty.SmallCheck   as SC
import           TestUtils
import           Translation.Translation
import           Typing.QType

translateStr :: LamRhoExp -> T.Text
translateStr = removeWhiteSpaces . T.pack . pyRenderStr . translate

compiStr :: String -> Either String (T.Text, T.Text)
compiStr src = pretty <$> compileStr src
  where
    pretty (typ, prog) = (T.pack typ, (removeWhiteSpaces . T.pack) prog)

compilerTests = testGroup "compilerTests"
  [ testCase "Lambda id" $
    compiStr "\\x.x" @?= Right ("$(1) -> (1)$", "lambda x: x")
  , testCase "zero" $
    compiStr "\\f. \\x. x" @?= Right ("$(1) -> ((1) -> (1))$", "lambda f: lambda x: x")
  , testCase "one church" $
    compiStr "\\f. \\x. f x" @?= Right ("$((1) -> (1)) -> ((1) -> (1))$", "lambda f: lambda x: (f)(x)")
  , testCase "add one one qubit" $
    compiStr "\\x.letcase y=\\pi^1 x in {\\ket{1}, \\ket{0}}" @?= Right
      ("$(1) -> (1)$","lambda x: letcase(x.measure(0),[lambda: "<>qubit1Str<>",lambda: "<>qubit0Str<>",])")
  ]
  where
    qubit1Str = translateStr (PQubits "1")
    qubit0Str = translateStr (PQubits "0")
