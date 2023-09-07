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
    runPy "print('Hello World')" >>= (@?= "Hello World\n")
  , testCase "Preamble test" $
    runPy "import preamble" >>= (@?= "")
  , testCase "PI0" $
    fullProg "\\pi^1 \\ket{0}" >>= runPy >>= (@?= "0\n")
  , testCase "PI1" $
    fullProg "\\pi^1 \\ket{1}" >>= runPy >>= (@?= "1\n")
  , testCase "PI00" $
    fullProg "\\pi^2 \\ket{00}" >>= runPy >>= (@?= "0\n")
  , testCase "PI01" $
    fullProg "\\pi^2 \\ket{01}" >>= runPy >>= (@?= "1\n")
  , testCase "PI10" $
    fullProg "\\pi^2 \\ket{10}" >>= runPy >>= (@?= "2\n")
  , testCase "PI11" $
    fullProg "\\pi^2 \\ket{11}" >>= runPy >>= (@?= "3\n")
  , testCase "PI111" $
    fullProg "\\pi^3 \\ket{111}" >>= runPy >>= (@?= "7\n")
  , testCase "PI110" $
    fullProg "\\pi^3 \\ket{110}" >>= runPy >>= (@?= "6\n")
  , testCase "PI1010" $
    fullProg "\\pi^4 \\ket{1010}" >>= runPy >>= (@?= "10\n")
  , testCase "00" $
    fullProg "\\ket{00}" >>= runPy >>= (@?= "{0: 1.0}\n")
  , testCase "11" $
    fullProg "\\ket{11}" >>= runPy >>= (@?= "{3: 1.0}\n")
  , testCase "+" $
    fullProg "\\ket{+}" >>= runPy >>= (@?= "{0: 0.5, 1: 0.5}\n")
  , testCase "-" $
    fullProg "\\ket{-}" >>= runPy >>= (@?= "{0: 0.5, 1: 0.5}\n")
  , testCase "lambda 11" $
    fullProg "\\pi^2 ((\\x. x) \\ket{11})" >>= runPy >>= (@?= "3\n")
  , testCase "add one one qubit zero" $
    fullProg "\\pi^1 ((\\x.letcase y=\\pi^1 x in {\\ket{1}, \\ket{0}}) \\ket{0})" >>= runPy >>= (@?= "1\n")
  , testCase "add one one qubit one" $
    fullProg "\\pi^1 ((\\x.letcase y=\\pi^1 x in {\\ket{1}, \\ket{0}}) \\ket{1})" >>= runPy >>= (@?= "0\n")
  ]
