{
module Parsing.LamRhoLexer where
import Control.Monad.Except
import CompilerError
}

%wrapper "basic"

$digit = 0-9
@prettynumber = $digit+ ([_] $digit+)*
@integer      = [\-]? @prettynumber
@decimal      = $digit+
@exponent     = [eE] [\-]? @decimal
@double       = [\-]? @decimal (\. @decimal)? @exponent?

$qubits = [01\+\-]
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  \\pi                          { \s -> TokenProjector }
  \\                            { \s -> TokenLambda }
  letcase                       { \s -> TokenLetCase }
  \.                            { \s -> TokenDot }
  \\ket\{$qubits+\}             { \s -> TokenQubits (stripSides s) }
  \\otimes                      { \s -> TokenOtimes }
  in                            { \s -> TokenIn }
  \=                            { \s -> TokenEq }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \{                            { \s -> TokenLBrace }
  \}                            { \s -> TokenRBrace }
  \[                            { \s -> TokenLBracket }
  \]                            { \s -> TokenRBracket }
  \,                            { \s -> TokenComma }
  \+                            { \s -> TokenPlus }
  \^                            { \s -> TokenPower }
  \_                            { \s -> TokenUnderscore }
  $lower+                       { \s -> TokenVar s }
  $upper+                       { \s -> TokenGate s }
  @decimal                      { \s -> TokenInt (read s) }
  @double                       { \s -> TokenDouble (read s) }

{

-- The token type:
data Token = TokenLambda
           | TokenVar String
           | TokenDot
           | TokenQubits String
           | TokenGate String
           | TokenOtimes
           | TokenProjector
           | TokenLetCase
           | TokenIn
           | TokenEq
           | TokenLParen
           | TokenRParen
           | TokenLBrace
           | TokenRBrace
           | TokenLBracket
           | TokenRBracket
           | TokenComma
           | TokenPower
           | TokenPlus
           | TokenUnderscore
           | TokenInt Int
           | TokenDouble Double
           deriving (Eq,Show)

stripSides :: String -> String
stripSides s = drop (length ("\\ket{"::String)) (init s)

-- https://www.haskell.org/alex/doc/html/basic-api.html
scanTokens :: String -> ExceptInfer [Token]
scanTokens str = go ('\n',[],str)
  where
    go :: AlexInput -> ExceptInfer [Token]
    go inp@(_,_bs,str) =
      case alexScan inp 0 of
       AlexEOF -> return []
       AlexError _ -> throwError $ ParsingError "Invalid lexeme when scanning"
       AlexSkip  inp' len     -> go inp'
       AlexToken inp' len act -> do
        res <- go inp'
        let rest = act (take len str)
        return (rest : res)
}
