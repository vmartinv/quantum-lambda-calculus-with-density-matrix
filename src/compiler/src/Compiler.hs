module Compiler(compile) where
import           Data.Text     (Text)
import           Lexer
import           Parser
import           Prettyprinter
import           Render
import           Translation


compile :: String -> Doc ann
compile = render . translate . parseTokens . scanTokens
