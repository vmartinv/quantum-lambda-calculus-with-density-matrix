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

pyOutput :: String -> IO String
pyOutput src = do
  (exit_code, out, err) <-
      readProcessWithExitCode "python" [] (fullProg src)
  exit_code @?= ExitSuccess
  err @?= ""
  return out

outMatchesRegex :: String -> String -> Assertion
outMatchesRegex src reg = do
  out <- pyOutput src
  let match = out =~ reg
  assertBool ("Output doesn't match regex.\nRegex:\n"++reg++"\nOutput:\n"++out) match

pythonTests = testGroup "pythonTests"
  [ testCase "Lambda id" $
    outMatchesRegex "\\x.x" "<function <lambda> at 0x[a-f0-9]+>"
  ]
