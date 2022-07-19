module ParserTests(parserTests) where
import           Control.Monad.Except
import           Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

parserTests :: TestTree
parserTests = testGroup "Lexer/Parser tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  []

qcProps = testGroup "(checked by QuickCheck)"
  []

testStr = runExcept.parseLambdaRho

unitTests = testGroup "Unit tests"
  [ testCase "Parsing var" $
      testStr "x" @?= Right (PVar "x")
  , testCase "Parsing gate with no arguments" $
      testStr "U x" @?= Right (PGate "U" [] (PVar "x"))
  , testCase "Parsing gate with single argument" $
      testStr "ZC^9 x" @?= Right (PGate "ZC" [9.0] (PVar "x"))
  , testCase "Parsing gate with int arguments" $
      testStr "I^{0,1} x" @?= Right (PGate "I" [0.0,1.0] (PVar "x"))
  , testCase "Parsing gate with negative argument" $
      testStr "UFZ^{-99} x" @?= Right (PGate "UFZ" [-99.0] (PVar "x"))
  , testCase "Parsing gate with negative arguments" $
      testStr "UFZ^{-3,-0, 1} x" @?= Right (PGate "UFZ" [-3.0, 0.0, 1.0] (PVar "x"))
  , testCase "Parsing gate with exp argument" $
      testStr "X^{1e3,1.1e3, -2.1e3, 4e-4, 4.4e-12, -5.03e-8} x" @?= Right (PGate "X" [1e3,1.1e3, -2.1e3, 4e-4, 4.4e-12, -5.03e-8] (PVar "x"))
  , testCase "Parsing gate with mixed arguments" $
      testStr "ASDFXX^{1e3,-3,4.4, 0} x" @?= Right (PGate "ASDFXX" [1e3,-3.0,4.4,0] (PVar "x"))
  , testCase "Parsing projector" $
      testStr "\\pi^10 x" @?= Right (PProjector 10 (PVar "x"))
  , testCase "Parsing single qubit" $
      testStr "|0>" @?= Right (PQubits "0")
  , testCase "Parsing multiple qubits" $
      testStr "|01+->" @?= Right (PQubits "01+-")
  , testCase "Parsing invalid qubits" $
      testStr "|2>" @?= Left "Invalid lexeme"
  , testCase "Parsing otimes" $
      testStr "x \\otimes y" @?= Right (PTimes (PVar "x") (PVar "y"))
  , testCase "Parsing lambda" $
      testStr "\\x.y" @?= Right (PLambda "x" (PVar "y"))
  , testCase "Parsing parenthesis" $
      testStr "(\\x.x)" @?= Right (PLambda "x" (PVar "x"))
  , testCase "Parsing function application" $
      testStr "(\\x.x) y" @?= Right (PFunApp (PLambda "x" (PVar "x")) (PVar "y"))
  , testCase "Parsing letcase" $
      testStr "letcase x=\\pi^5 y in {x,y}" @?= Right (PLetCase "x" (PProjector 5 (PVar "y")) [(PVar "x"), (PVar "y")])
  , testCase "Parsing unknown token" $
      testStr "x | y" @?= Left "Invalid lexeme"
  , testCase "Parsing unfinished expression" $
      testStr "x \\otimes" @?= Left "Unexpected end of Input"
  , testCase "Parsing unfinished expression gate" $
      testStr "U" @?= Left "Unexpected end of Input"
  , testCase "Parsing invalid expression" $
      testStr "\\ |+>" @?= Left "TokenQubits \"+\""
  , testCase "Parsing nested letcase" $
      testStr "\\x.\\y. letcase ym=\\pi^1 (letcase zz=\\pi^3 (x \\otimes y) in {|0>, |0>, |0>, |0>, |0>, |0>, |0>, |0>}) in {|1>, |+>}"
        @?=  Right (PLambda "x" (PLambda "y" (PLetCase "ym" (PProjector 1 (PLetCase "zz" (PProjector 3 (PTimes (PVar "x") (PVar "y"))) [PQubits "0",PQubits "0",PQubits "0",PQubits "0",PQubits "0",PQubits "0",PQubits "0",PQubits "0"])) [PQubits "1",PQubits "+"])))
  ]
