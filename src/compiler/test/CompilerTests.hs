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
  , testCase "Lambda id program" $
    makeProgram "lambda x: x" @?= "from preamble import *\n\nprog = lambda x: x\n\nprint(prog)"
  , testCase "invalid" $
    compiStr "\\f. \\x. z" @?= Left "UnboundVariable \"z\""
  , testCase "zero" $
    compiStr "\\f. \\x. x" @?= Right ("$(1) -> ((1) -> (1))$", "lambda f: lambda x: x")
  , testCase "otimes simple" $
    compiStr "\\ket{0} \\otimes \\ket{1}" @?= Right ("$(2)$","Circuit([1.0, 0.0, 0.0, 0.0,]).compose(Circuit([0.0, 0.0, 0.0, 1.0,]))")
  , testCase "otimes gate" $
    compiStr "\\ket{0} \\otimes (U^{1,2,4} \\ket{1})" @?= Right ("$(2)$","Circuit([1.0, 0.0, 0.0, 0.0,]).compose(Circuit([0.0, 0.0, 0.0, 1.0,]).u(1.0,2.0,4.0,0))")
  , testCase "gate var" $
    compiStr "\\x. U^{1,2,4} x" @?= Right ("$(1) -> (1)$","lambda x: x.u(1.0,2.0,4.0,0)")
  , testCase "gate var app" $
    compiStr "\\x. (\\y.y) (U^{1,2,4} x)" @?= Right ("$(1) -> (1)$","lambda x: (lambda y: y)(x.u(1.0,2.0,4.0,0))")
  , testCase "gate var app otimes" $
    compiStr "(\\x. \\ket{0} \\otimes U^{1,2,4} x) \\ket{0}" @?= Right ("$(2)$","(lambda x: Circuit([1.0, 0.0, 0.0, 0.0,]).compose(x.u(1.0,2.0,4.0,0)))(Circuit([1.0, 0.0, 0.0, 0.0,]))")
  , testCase "gate" $
    compiStr "CU^{1,2,3,4}_1 \\ket{011}" @?= Right ("$(3)$","Circuit([0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,]).cu(1.0,2.0,3.0,4.0,2,4)")
  , testCase "Lambda otimes gate" $
    compiStr "\\x.\\y. (CU^{1,2,3,4}_1 (x \\otimes y))" @?= Right ("$(2) -> ((1) -> (3))$","lambda x: lambda y: x.compose(y).cu(1.0,2.0,3.0,4.0,2,4)")
  , testCase "Lambda otimes" $
    compiStr "\\x.\\y. (x \\otimes y)" @?= Right ("$(1) -> ((1) -> (2))$","lambda x: lambda y: x.compose(y)")
  , testCase "one church" $
    compiStr "\\f. \\x. f x" @?= Right ("$((1) -> (1)) -> ((1) -> (1))$", "lambda f: lambda x: (f)(x)")
  , testCase "add one one qubit" $
    compiStr "\\x.letcase y=\\pi^1 x in {\\ket{1}, \\ket{0}}" @?= Right
      ("$(1) -> (1)$","lambda x: letcase((lambda _rho: (lambda _r: (((_r) // (2**(((_rho.size()) // (2)) - (1))), 1), Circuit.fromInt((_rho.size()) // (2),_r)))(_rho.measure(*range(0,_rho.size(),2))))(x),[lambda y: Circuit([0.0, 0.0, 0.0, 1.0,]),lambda y: Circuit([1.0, 0.0, 0.0, 0.0,]),])")
  , testCase "pair" $
    compiStr "(0^1, [[1,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]])" @?= Right
      ("$(1, 2)$","((0, 1), Circuit([1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,]))")
  ]
  where
    qubit1Str = translateStr (PQubits "1")
    qubit0Str = translateStr (PQubits "0")
