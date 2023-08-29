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

fullProg src = (makeProgram . snd . fromRight ("", "") . compileStr) src

runPy :: String -> IO String
runPy prog = do
  (exitCode, out, err) <-
      readProcessWithExitCode "python" [] prog
  err @?= ""
  exitCode @?= ExitSuccess
  return out

matchesRegex :: String -> String -> Assertion
matchesRegex reg out =
    assertBool ("Output doesn't match regex.\nOutput:\n"++out++"\nRegex:\n"++reg) match
  where
    match = out =~ reg

pythonTests = testGroup "pythonTests"
  [ testCase "Lambda id" $
    runPy (fullProg "\\x.x") >>= matchesRegex "<function <lambda> at 0x[a-f0-9]+>"
  ]
