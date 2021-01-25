{
module Grammar where
import Tokens

import Data.Text (Text, pack)
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    '\\' { TokenLambda }
    var { TokenVar $$ }
    '.' { TokenDot }
    qubits { TokenQubits $$ }
    gate { TokenGate $$ }
    '*' { TokenTimes }
    PI { TokenProjector }
    letcase { TokenLetCase }
    in { TokenIn }
    '=' { TokenEq }
    '(' { TokenLParen }
    ')' { TokenRParen }
    '{' { TokenLBrace }
    '}' { TokenRBrace }
    ',' { TokenComma }

%right in
%left '*' ','

%%

PExp : var                    { PVar (pack $1) }
    | '\\' var '.' PExp       { PLambda (pack $2) $4 }
    | PExp PExp               { PFunApp $1 $2 }
    | qubits                  { PQubits (pack $1) }
    | gate PExp               { PGate (pack $1) $2 }
    | PI PExp                 { PProjector $2 }
    | PExp '*' PExp           { PTimes $1 $3 }
    | '(' PExp ')'            { $2 }
    | letcase var '=' PExp in '{' CaseList '}' { PLetCase (pack $2) $4 (reverse $7) }

CaseList : PExp              { [$1] }
         | CaseList ',' PExp { $3 : $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data PExp = PVar Text
         | PLambda Text PExp
         | PFunApp PExp PExp
         | PQubits Text
         | PGate Text PExp
         | PProjector PExp
         | PTimes PExp PExp
         | PLetCase Text PExp [PExp]
         deriving (Show,Eq)
}