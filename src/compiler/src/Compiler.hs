module Compiler(makeProgram, compile, compileStr) where
import           CompilerError
import           Control.Monad.Except
import           Parsing.LamRhoParser
import           Python.PyExp
import           Python.PyRender
import           Translation.Translation
import           Typing.QType
import           Typing.TypeChecker
import           Utils


makeProgram :: String -> String
makeProgram body = preamble ++ prog ++ ending
  where
    preamble = "from preamble import *\n\n"
    prog = "prog = " ++ body ++ "\n\n"
    ending = "print(prog.measure_all())"

compile :: String -> ExceptInfer (QType, PyExp)
compile src = do
  exp <- parseLambdaRho src
  typ <- typeCheck exp
  return (typ, translate exp)

compileStr :: String -> Either String (String, String)
compileStr src = either showError showResult $ runExcept (compile src)
    where
      showError msg = Left $ show msg
      showResult (typ, prog) = Right (show typ, pyRenderStr prog)
