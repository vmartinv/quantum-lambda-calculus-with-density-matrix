import           Parsing.LamRhoParserTests     (lamRhoParserTests)
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
tests = testGroup "tests" [lamRhoParserTests, qTypeTests, typeCheckTests, purificationTests, utilsTests, stateBuilderTests]
