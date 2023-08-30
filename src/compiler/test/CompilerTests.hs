module CompilerTests(compilerTests) where
import           Compiler
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC
import           Typing.QType

compilerTests = testGroup "compilerTests"
  [ testCase "Lambda id" $
    compileStr "\\x.x" @?= Right ("$(1) -> (1)$", "lambda x: x")
  , testCase "zero" $
      compileStr "\\f. \\x. x" @?= Right ("$(1) -> ((1) -> (1))$", "lambda f: lambda x: x")
  , testCase "one church" $
      compileStr "\\f. \\x. f x" @?= Right ("$((1) -> (1)) -> ((1) -> (1))$", "lambda f: lambda x: (f)(x)")
  , testCase "add one one qubit" $
      compileStr "\\x.letcase y=\\pi^1 x in {\\ket{1}, \\ket{0}}" @?= Right ("$((1) -> (1)) -> ((1) -> (1))$", "lambda f: lambda x: (f)((f)(x))")
  -- , testCase "add one two qubits" $
  --     compileStr "\\x.letcase y=\\pi^2 x in {\\ket{01}, \\ket{10}, \\ket{11}, \\ket{00}}" @?= Right ("$((1) -> (1)) -> ((1) -> (1))$", "lambda f: lambda x: (f)((f)(x))")
  -- , testCase "add one three qubits" $
  --     compileStr "\\x.letcase y=\\pi^3 x in {\\ket{001}, \\ket{010}, \\ket{011}, \\ket{100}, \\ket{101}, \\ket{110}, \\ket{111}, \\ket{000}}" @?= Right ("$((1) -> (1)) -> ((1) -> (1))$", "lambda f: lambda x: (f)((f)(x))")
  ]
