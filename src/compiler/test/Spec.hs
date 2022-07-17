import           ParserTests           (parserTests)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Typing.QTypeTests     (qTypeTests)
import           Typing.TypeCheckTests (typeCheckTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [parserTests, qTypeTests, typeCheckTests]
