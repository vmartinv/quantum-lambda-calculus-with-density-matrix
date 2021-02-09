import           ParserTests      (parserTests)
import           QTypeTests       (qTypeTests)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TypeCheckTests   (typeCheckTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [parserTests, qTypeTests, typeCheckTests]
