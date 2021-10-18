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
  , testCase "Parsing gates" $
      testStr "U x" @?= Right (PGate "U" (PVar "x"))
  , testCase "Parsing projector" $
      testStr "\\pi x" @?= Right (PProjector (PVar "x"))
  , testCase "Parsing single qubit" $
      testStr "|0>" @?= Right (PQubits "0")
  , testCase "Parsing multiple qubits" $
      testStr "|01+->" @?= Right (PQubits "01+-")
  , testCase "Parsing otimes" $
      testStr "x * y" @?= Right (PTimes (PVar "x") (PVar "y"))
  , testCase "Parsing lambda" $
      testStr "\\x.y" @?= Right (PLambda "x" (PVar "y"))
  , testCase "Parsing parenthesis" $
      testStr "(\\x.x)" @?= Right (PLambda "x" (PVar "x"))
  , testCase "Parsing function application" $
      testStr "(\\x.x) y" @?= Right (PFunApp (PLambda "x" (PVar "x")) (PVar "y"))
  , testCase "Parsing letcase" $
      testStr "letcase x=\\pi y in {x,y}" @?= Right (PLetCase "x" (PProjector (PVar "y")) [(PVar "x"), (PVar "y")])
  , testCase "Parsing unknown token" $
      testStr "x | y" @?= Left "Invalid lexeme"
  , testCase "Parsing unfinished expression" $
      testStr "x *" @?= Left "Unexpected end of Input"
  , testCase "Parsing unfinished expression gate" $
      testStr "U" @?= Left "Unexpected end of Input"
  , testCase "Parsing invalid expression" $
      testStr "\\ |+>" @?= Left "TokenQubits \"+\""
  ]
