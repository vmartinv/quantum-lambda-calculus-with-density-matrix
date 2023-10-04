{
module Parsing.LamRhoParser where
import Parsing.LamRhoLexer
import Control.Monad.Except
import Parsing.LamRhoExp
import CompilerError

import Data.Complex
import Data.Text (pack)
}

%name parseTokens
%tokentype { Token }
%monad { ExceptInfer } { (>>=) } { return }
%error { parseError }

%token
    '\\' { TokenLambda }
    'i' { TokenVar "i" }
    var { TokenVar $$ }
    '.' { TokenDot }
    qubits { TokenQubits $$ }
    gate { TokenGate $$ }
    OTIMES { TokenOtimes }
    PI { TokenProjector }
    letcase { TokenLetCase }
    in { TokenIn }
    '=' { TokenEq }
    '(' { TokenLParen }
    ')' { TokenRParen }
    '{' { TokenLBrace }
    '}' { TokenRBrace }
    '[' { TokenLBracket }
    ']' { TokenRBracket }
    ',' { TokenComma }
    '^' { TokenPower }
    '_' { TokenUnderscore }
    '+' { TokenPlus }
    int { TokenInt $$ }
    double { TokenDouble $$ }

%nonassoc MAT GAT PROJ
%left OTIM
%left APP
%nonassoc LAMB
%nonassoc '[' ']' '{' '}' '(' ')'
%nonassoc var 'i' PI '\\' qubits gate letcase OTIMES ','

%%

-- pack :: String -> Text

LamRhoExp : var                              { PVar (pack $1) }
    | 'i'                                    { PVar "i" }
    | PI '^' int LamRhoExp %prec PROJ        { PProjector $3 $4 }
    | '\\' 'i' '.' LamRhoExp %prec LAMB      { PLambda "i" $4 }
    | '\\' var '.' LamRhoExp %prec LAMB      { PLambda (pack $2) $4 }
    | LamRhoExp LamRhoExp %prec APP          { PFunApp $1 $2 }
    | qubits                                 { PQubits (pack $1) }
    | '(' int '^' int ',' '[' Matrix ']' ')' { PPair $2 $4 (reverse $7) }
    | '[' Matrix ']'  %prec MAT              { PMatrix (reverse $2) }
    | GateP LamRhoExp %prec GAT              { PGateApp $1 $2 }
    | LamRhoExp OTIMES LamRhoExp %prec OTIM  { POtimesExp $1 $3 }
    | '(' LamRhoExp ')'                      { $2 }
    | letcase var '=' LamRhoExp in '{' CaseList '}' { PLetCase (pack $2) $4 (reverse $7) }

Gate : gate '^' NumExp                { PGate (pack $1) [$3] }
    | gate                            { PGate (pack $1) [] }
    | gate '^' '{' NumberList '}'     { PGate (pack $1) (reverse $4) }

GateP : Gate                          { $1 0 }
      | Gate '_' int                  { $1 $3 }

NumberList : NumExp                   { [$1] }
    | NumberList ',' NumExp           { $3 : $1 }

NumExp : double                       { $1 }
  | int                               { fromIntegral $1 }

Matrix : '[' ComplexList ']'          { [reverse $2] }
  | Matrix ',' '[' ComplexList ']'    { (reverse $4) : $1 }

ComplexList : ComplexExp              { [$1] }
  | ComplexList ',' ComplexExp        { $3 : $1 }

ComplexExp : NumExp                   { $1 :+ 0}
  | 'i'                               { 0 :+ 1 }
  | NumExp 'i'                        { 0 :+ $1 }
  | NumExp '+' 'i'                    { $1 :+ 1 }
  | NumExp '+' NumExp 'i'             { $1 :+ $3 }

CaseList : LamRhoExp              { [$1] }
         | CaseList ',' LamRhoExp { $3 : $1 }

{
parseError :: [Token] -> ExceptInfer a
parseError (l:ls) = throwError $ ParsingError ("Unexpected lexeme: "<>show l)
parseError [] = throwError $ ParsingError "Unexpected end of Input"

parseLambdaRho :: String -> ExceptInfer LamRhoExp
parseLambdaRho =  scanTokens >=> parseTokens

}
