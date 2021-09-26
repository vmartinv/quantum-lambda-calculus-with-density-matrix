module Compiler(compile) where
import           Control.Monad.Except
import           Data.Text            (Text)
import           Parser
import           Prettyprinter
import           Render
import           Translation
import           Typing.QType
import           Typing.TypeChecker


compile :: String -> Except String (QType, Doc ann)
compile src = do
  exp <- parseLambdaRho src
  typ <- typeCheck exp
  let result = render (translate exp)
  return (typ, result)
