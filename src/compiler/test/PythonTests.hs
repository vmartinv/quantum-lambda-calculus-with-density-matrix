module PythonTests(pythonTests) where
import           Compiler
import           Data.Either
import           System.Exit
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC
import           Text.Regex.TDFA

fullProg :: String -> IO String
fullProg src = do
  either handleError handleResult (compileStr src)
  where
    handleResult = return . makeProgram . snd
    handleError msg = assertFailure ("Error while compiling: "++msg++"\nSource:\n"++src) >> return ""

runPy :: String -> IO String
runPy prog = do
  (exitCode, out, err) <-
      readProcessWithExitCode "python" [] prog
  assertBool ("Error while running program: "++err++"\nCompiled Python code:\n"++prog) (err=="")
  exitCode @?= ExitSuccess
  return out

matchesRegex :: String -> String -> Assertion
matchesRegex reg out =
    assertBool ("Output doesn't match regex.\nOutput:\n"++out++"Regex:\n"++reg) match
  where
    match = out =~ reg

pythonTests = testGroup "pythonTests"
  [ testCase "zero" $
    fullProg "\\ket{0}" >>= runPy >>= matchesRegex "0"
  ]
