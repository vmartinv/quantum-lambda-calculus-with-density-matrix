module Compiler(makeProgram, compile, compileStr) where
import           Control.Monad.Except
import           Parsing.LamRhoParser
import           Python.PyExp
import           Python.PyRender
import           Translation.Translation
import           Typing.QType
import           Typing.TypeChecker


makeProgram :: String -> String
makeProgram body = preamble ++ prog ++ ending
  where
    preamble = "from preamble import *\n\n\n"
    prog = "prog = " ++ body ++ "\n\n"
    ending = "print(prog)"

compile :: String -> Except String (QType, PyExp)
compile src = do
  exp <- parseLambdaRho src
  typ <- typeCheck exp
  return (typ, translate exp)

compileStr :: String -> Either String (String, String)
compileStr src = runExcept (toStr <$> compile src)
    where
      toStr (typ, prog) = (show typ, pyRenderStr prog)
