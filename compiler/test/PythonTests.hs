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
import           Parsing.LamRhoParser
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
  , multiTests
  ]

basicTests = testGroup "basicTests"
  [ testCase "Python test" $
    runPy "print('Hello World')" >>= (@?= "Hello World\n")
  , testCase "Preamble test" $
    runPy "import preamble" >>= (@?= "")
  , testCase "PI0" $
    fullProg "\\pi^1 \\ket{0}" >>= runPy >>= isClassicMeasurement 0
  , testCase "pair letcase" $
    fullProg "letcase x=(0^1, [[1,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]) in {x, \\ket{11}}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,1.0)]))
  , testCase "pair letcase 1" $
    fullProg "letcase x=(1^1, [[1,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]) in {x, \\ket{11}}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(3,1.0)]))
  , testCase "PI1" $ 
    fullProg "\\pi^1 \\ket{1}" >>= runPy >>= isClassicMeasurement 1
  , testCase "PI00" $
    fullProg "\\pi^2 \\ket{00}" >>= runPy >>= isClassicMeasurement 0
  , testCase "PI01" $
    fullProg "\\pi^2 \\ket{01}" >>= runPy >>= isClassicMeasurement 1
  , testCase "PI10" $
    fullProg "\\pi^2 \\ket{10}" >>= runPy >>= isClassicMeasurement 2
  , testCase "PI11" $
    fullProg "\\pi^2 \\ket{11}" >>= runPy >>= isClassicMeasurement 3
  , testCase "PI111" $
    fullProg "\\pi^3 \\ket{111}" >>= runPy >>=isClassicMeasurement 7
  , testCase "PI110" $
    fullProg "\\pi^3 \\ket{110}" >>= runPy >>= isClassicMeasurement 6
  , testCase "PI1010" $
    fullProg "\\pi^4 \\ket{1010}" >>= runPy >>= isClassicMeasurement 10
  , testCase "00" $
    fullProg "\\ket{00}" >>= runPy >>= (@?= "{\"0\": 1.0}\n")
  , testCase "11" $
    fullProg "\\ket{11}" >>= runPy >>= (@?= "{\"3\": 1.0}\n")
  , testCase "plus" $
    fullProg "\\ket{+}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,0.5),(1,0.5)]))
  , testCase "doubleplus" $
      fullProg "\\ket{++}"
        >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,0.25), (1,0.25), (2,0.25), (3,0.25)]))
  , testCase "minus" $
    fullProg "\\ket{-}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,0.5),(1,0.5)]))
  , testCase "plusplus" $
    fullProg "\\ket{++}" >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(0,0.25),(1,0.25),(2,0.25),(3,0.25)]))
  , testCase "lambda 11" $
    fullProg "\\pi^2 ((\\x. x) \\ket{11})" >>= runPy >>= isClassicMeasurement 3
  ]

gateTests = testGroup "gateTests"
  [ testCase "x1" $
    fullProg ("\\pi^1 "<>gateX 0<>"\\ket{1}") >>= runPy >>= isClassicMeasurement 0
  , testCase "x000" $
    fullProg ("\\pi^3 "<>gateX 2<>gateX 1<>" \\ket{000}") >>= runPy >>= isClassicMeasurement 6
  , testCase "h0" $
    fullProg (gateH 0<>" \\ket{110}") >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(6,0.5),(7,0.5)]))
  , testCase "h0letcase" $
    fullProg ("\\pi^2 (letcase y=\\pi^1 "<>gateH 0<>" \\ket{0} in {\\ket{11}, \\ket{11}})") >>= runPy >>= isClassicMeasurement 3
  , testCase "cnot1" $
    fullProg ("\\pi^3 "<>gateCNOT 1<>" \\ket{110}") >>= runPy >>= isClassicMeasurement 2
  , testCase "cnot0" $
    fullProg ("\\pi^3 "<>gateCNOT 1<>" \\ket{010}") >>= runPy >>= isClassicMeasurement 6
  , testCase "swap" $
    fullProg ("\\pi^2 SWAP_0 \\ket{01}") >>= runPy >>= isClassicMeasurement 2
  ]
  where
    gateX p = "U^{3.14159265359, 0, 3.14159265359}_"<>show p
    gateH p = "U^{1.5707963268, 0, 3.14159265359}_"<>show p
    gateCNOT p = "CU^{3.14159265359, 0, 3.14159265359, 0}_"<>show p

multiTests = testGroup "multiTests"
  [ testCase "lambda" $
    fullProg ("\\pi^1 ((\\x.\\y.x) \\ket{0} \\ket{1})") >>= runPy >>= isClassicMeasurement 0
  , testCase "otimes1" $
      fullProg "\\ket{0} \\otimes \\ket{1}"
        >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(1,1.0)]))
  , testCase "otimes2" $
      fullProg "\\ket{10} \\otimes \\ket{1}"
        >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(5,1.0)]))
  , testCase "Projection on otimes with letcase" $
      fullProg "letcase ym=\\pi^3 (\\ket{11} \\otimes \\ket{0}) in {\\ket{000}, \\ket{001}, \\ket{010}, \\ket{011}, \\ket{100}, \\ket{101}, \\ket{110}, \\ket{111}}  "
        >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(6,1.0)]))
  , testCase "Projection on 2 unknowns with letcase" $
      fullProg "(\\x.\\y. letcase ym=\\pi^3 (x \\otimes y) in {\\ket{000}, \\ket{001}, \\ket{010}, \\ket{011}, \\ket{100}, \\ket{101}, \\ket{110}, \\ket{111}}) \\ket{10} \\ket{0}"
        >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(4,1.0)]))
  , testCase "Projection on 2 unknowns with nested letcase" $
      fullProg "(\\x.\\y. letcase ym=\\pi^3 (x \\otimes y) in {\\ket{000}, \\ket{001}, \\ket{010}, letcase il=\\pi^1\\ket{+} in {\\ket{001}, \\ket{001}}, \\ket{100}, \\ket{101}, \\ket{110}, \\ket{111}}) \\ket{0} \\ket{11}"
        >>= runPy >>= decodeMeasurement >>= st . (approxEqualMeasurement (M.fromList [(1,1.0)]))
  ]
