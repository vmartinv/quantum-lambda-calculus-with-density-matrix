module Compiler(compile) where
import           Control.Monad.Except
import           Data.Text               (Text)
import           Parsing.Parser
import           Prettyprinter
import           Python.PyRender
import           Translation.Translation
import           Typing.QType
import           Typing.TypeChecker


compile :: String -> Except String (QType, Doc ann)
compile src = do
  exp <- parseLambdaRho src
  typ <- typeCheck exp
  let result = pyRender (translate exp)
  return (typ, result)
