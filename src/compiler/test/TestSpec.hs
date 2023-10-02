import           CompilerTests                 (compilerTests)
import           Parsing.LamRhoParserTests     (lamRhoParserTests)
import           PythonTests                   (pythonTests)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Translation.PurificationTests (purificationTests)
import           Translation.TranslationTests  (translationTests)
import           Typing.QTypeTests             (qTypeTests)
import           Typing.TypeCheckTests         (typeCheckTests)
import           UtilsTests                    (utilsTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
    utilsTests,
    lamRhoParserTests,
    qTypeTests,
    typeCheckTests,
    purificationTests,
    translationTests,
    compilerTests
    --, pythonTests
  ]
