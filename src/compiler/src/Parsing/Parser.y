{
module Parsing.Parser where
import Parsing.Lexer
import Control.Monad.Except
import Parsing.PExp

import Data.Text (pack)
}

%name parseTokens
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
    '\\' { TokenLambda }
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
    int { TokenInt $$ }
    double { TokenDouble $$ }

%nonassoc '[' ']'
%nonassoc '{' '}'
%nonassoc '(' ')'
%nonassoc MAT GAT PROJ OTIM APP LAMB
%nonassoc var PI '\\' qubits gate letcase
%left OTIMES ','

%%

-- pack :: String -> Text

PExp : var                              { PVar (pack $1) }
    | PI '^' int PExp %prec PROJ        { PProjector $3 $4 }
    | '\\' var '.' PExp %prec LAMB      { PLambda (pack $2) $4 }
    | PExp PExp %prec APP               { PFunApp $1 $2 }
    | qubits                            { PQubits (pack $1) }
    | '[' Matrix ']'  %prec MAT         { PMatrix (reverse $2) }
    | Gate PExp %prec GAT               { PGateApp $1 $2 }
    | PExp OTIMES PExp %prec OTIM       { POtimesExp $1 $3 }
    | '(' PExp ')'                      { $2 }
    | letcase var '=' PExp in '{' CaseList '}' { PLetCase (pack $2) $4 (reverse $7) }

Matrix : '[' NumberList ']'              { [reverse $2] }
    | Matrix ',' '[' NumberList ']'      { (reverse $4) : $1 }

Gate : gate '^' NumExp                { PGate (pack $1) [$3] }
    | gate                            { PGate (pack $1) [] }
    | gate '^' '{' NumberList '}'  { PGate (pack $1) (reverse $4) }
    | Gate OTIMES Gate                { PGateOtimes $1 $3 }
    | '(' Gate ')'                    { $2 }

NumberList : NumExp                   { [$1] }
    | NumberList ',' NumExp           { $3 : $1 }

NumExp : double                       { $1 }
  | int                               { fromIntegral $1 }

CaseList : PExp              { [$1] }
         | CaseList ',' PExp { $3 : $1 }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError ("Unexpected lexeme: "<>show l)
parseError [] = throwError "Unexpected end of Input"

parseLambdaRho :: String -> Except String PExp
parseLambdaRho =  scanTokens >=> parseTokens

}
