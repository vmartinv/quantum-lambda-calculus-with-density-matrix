module PythonTests(pythonTests) where
import           Compiler
import           Data.Either
import           System.Exit
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC
import           Utils

fullProg :: String -> IO String
fullProg src = do
  either handleError handleResult (compileStr src)
  where
    handleResult = return . dprint "fullProg" . makeProgram . snd
    handleError msg = assertFailure ("Error while compiling: "++msg++"\nSource:\n"++src) >> return ""

runPy :: String -> IO String
runPy prog = do
  (exitCode, out, err) <-
      readProcessWithExitCode "python" [] prog
  assertBool ("Error while running program: "++err++"\nStdout:\n"++out++"\nCompiled Python code:\n"++prog) (err=="")
  exitCode @?= ExitSuccess
  return out

pythonTests = testGroup "pythonTests"
  [ testCase "Python test" $
    runPy "print(\"Hello World\")" >>= (@?= "Hello World\n")
  , testCase "zero" $
    fullProg "\\ket{0}" >>= runPy >>= (@?= "0\n")
  , testCase "one" $
    fullProg "\\ket{1}" >>= runPy >>= (@?= "1\n")
  ]
