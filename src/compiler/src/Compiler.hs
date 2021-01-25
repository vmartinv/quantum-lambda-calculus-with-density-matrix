module Compiler(compile) where
import           Data.Text     (Text)
import           Grammar
import           Prettyprinter
import           Render
import           Tokens
import           Translation


compile :: String -> Doc ann
compile = render.translate.parseTokens.scanTokens
