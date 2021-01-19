{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$qubits = [01\+\-]
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  \\                            { \s -> TokenLambda }
  PI                            { \s -> TokenProjector }
  letcase                       { \s -> TokenLetCase }
  \.                            { \s -> TokenDot }
  \|$qubits+\>                  { \s -> TokenQubits (stripSides s) }
  \*                            { \s -> TokenTimes }
  in                            { \s -> TokenIn }
  \=                            { \s -> TokenEq }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \{                            { \s -> TokenLBrace }
  \}                            { \s -> TokenRBrace }
  \,                            { \s -> TokenComma }
  $lower+                       { \s -> TokenVar s }
  $upper+                       { \s -> TokenGate s }

{

-- The token type:
data Token = TokenLambda
           | TokenVar String
           | TokenDot
           | TokenQubits String
           | TokenGate String
           | TokenTimes
           | TokenProjector
           | TokenLetCase
           | TokenIn
           | TokenEq
           | TokenLParen
           | TokenRParen
           | TokenLBrace
           | TokenRBrace
           | TokenComma
           deriving (Eq,Show)

stripSides :: String -> String
stripSides s = tail (init s)

scanTokens = alexScanTokens

}
