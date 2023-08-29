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
  ]
