module Compiler(compile) where
import           Control.Monad.Except
import           Data.Text               (Text)
import           Parsing.LamRhoParser
import           Python.PyExp
import           Translation.Translation
import           Typing.QType
import           Typing.TypeChecker


compile :: String -> Except String (QType, PyExp)
compile src = do
  exp <- parseLambdaRho src
  typ <- typeCheck exp
  return (typ, translate exp)
