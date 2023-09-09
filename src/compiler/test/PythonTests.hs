module PythonTests(pythonTests) where
import           Compiler
import           Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either
import qualified Data.Map                   as M
import           System.Exit
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      as QC
import           Test.Tasty.SmallCheck      as SC
import           TestUtils
import           Utils


fullProg :: String -> IO String
fullProg src =
  either handleError handleResult (compileStr src)
  where
    handleResult = return . dprint "fullProg" . makeProgram . snd
    handleError msg = assertFailure ("Error while compiling: "++msg++"\nSource:\n"++src) >> return ""

decodeMeasurement :: String -> IO (M.Map Int Double)
decodeMeasurement s =
  maybe handleError handleResult (JSON.decode (BS.pack s))
  where
    handleResult = return
    handleError = assertFailure ("Can't parse measurement: "++s) >> return (M.fromList [])

runPy :: String -> IO String
runPy prog = do
  (exitCode, out, err) <-
      readProcessWithExitCode "python" [] prog
  assertBool ("Error while running program: "++err++"\nStdout:\n"++out++"\nCompiled Python code:\n"++prog) (err=="")
  exitCode @?= ExitSuccess
  return out

pythonTests = testGroup "pythonTests"
  [ basicTests
  , gateTests
  ]

basicTests = testGroup "basicTests"
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
    fullProg "\\ket{00}" >>= runPy >>= (@?= "{\"0\": 1.0}\n")
  , testCase "11" $
    fullProg "\\ket{11}" >>= runPy >>= (@?= "{\"3\": 1.0}\n")
  , testCase "plus" $
    fullProg "\\ket{+}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,0.5),(1,0.5)]))
  , testCase "minus" $
    fullProg "\\ket{-}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,0.5),(1,0.5)]))
  , testCase "plusplus" $
    fullProg "\\ket{++}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,0.25),(1,0.25),(2,0.25),(3,0.25)]))
  , testCase "lambda 11" $
    fullProg "\\pi^2 ((\\x. x) \\ket{11})" >>= runPy >>= (@?= "3\n")
  , testCase "add one one qubit zero" $
    fullProg "\\pi^1 ((\\x.letcase y=\\pi^1 x in {\\ket{1}, \\ket{0}}) \\ket{0})" >>= runPy >>= (@?= "1\n")
  , testCase "add one one qubit one" $
    fullProg "\\pi^1 ((\\x.letcase y=\\pi^1 x in {\\ket{1}, \\ket{0}}) \\ket{1})" >>= runPy >>= (@?= "0\n")
  ]

gateTests = testGroup "gateTests"
  [ testCase "x1" $
    fullProg ("\\pi^1 "<>gateX 0<>"\\ket{1}") >>= runPy >>= (@?= "0\n")
  , testCase "x000" $
    fullProg ("\\pi^3 "<>gateX 2<>gateX 1<>" \\ket{000}") >>= runPy >>= (@?= "6\n")
  , testCase "h0" $
    fullProg (gateH 0<>" \\ket{110}") >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(6,0.5),(7,0.5)]))
  , testCase "h0letcase" $
    fullProg ("\\pi^2 (letcase y=\\pi^1 "<>gateH 0<>" \\ket{0} in {\\ket{11}, \\ket{11}})") >>= runPy >>= (@?= "3\n")
  , testCase "cnot1" $
    fullProg ("\\pi^3 "<>gateCNOT 1<>" \\ket{110}") >>= runPy >>= (@?= "2\n")
  , testCase "cnot0" $
    fullProg ("\\pi^3 "<>gateCNOT 1<>" \\ket{010}") >>= runPy >>= (@?= "6\n")
  , testCase "swap" $
    fullProg ("\\pi^2 SWAP_0 \\ket{01}") >>= runPy >>= (@?= "2\n")
  , testCase "lambda" $
    fullProg ("\\pi^1 ((\\x.\\y.x) \\ket{0} \\ket{1})") >>= runPy >>= (@?= "0\n")
  ]
  where
    gateX p = "U^{3.14159265359, 0, 3.14159265359}_"<>show p
    gateH p = "U^{1.5707963268, 0, 3.14159265359}_"<>show p
    gateCNOT p = "CU^{3.14159265359, 0, 3.14159265359, 0}_"<>show p
