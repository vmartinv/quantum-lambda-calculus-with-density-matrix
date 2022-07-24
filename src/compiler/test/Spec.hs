import           Parsing.ParserTests           (parserTests)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Translation.PurificationTests (purificationTests)
import           Translation.StateBuilderTests (stateBuilderTests)
import           Typing.QTypeTests             (qTypeTests)
import           Typing.TypeCheckTests         (typeCheckTests)
import           UtilsTests                    (utilsTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [parserTests, qTypeTests, typeCheckTests, purificationTests, utilsTests, stateBuilderTests]
