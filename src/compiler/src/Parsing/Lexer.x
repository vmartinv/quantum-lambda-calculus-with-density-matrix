{
module Parsing.Lexer where
import Control.Monad.Except
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
  \|$qubits+\>                  { \s -> TokenQubits (stripSides s) }
  \\otimes                      { \s -> TokenOtimes }
  in                            { \s -> TokenIn }
  \=                            { \s -> TokenEq }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \{                            { \s -> TokenLBrace }
  \}                            { \s -> TokenRBrace }
  \,                            { \s -> TokenComma }
  $lower+                       { \s -> TokenVar s }
  $upper+                       { \s -> TokenGate s }
  \^                            { \s -> TokenPower }
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
           | TokenComma
           | TokenPower
           | TokenInt Int
           | TokenDouble Double
           deriving (Eq,Show)

stripSides :: String -> String
stripSides s = tail (init s)

-- https://www.haskell.org/alex/doc/html/basic-api.html
scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme"
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)
}
