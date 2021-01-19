{
module Grammar where
import Tokens
}

%name parseLambdaRhoTokens
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

PExp : var                    { PVar $1 }
    | '\\' var '.' PExp       { PLambda $2 $4 }
    | PExp PExp               { PFunApp $1 $2 }
    | qubits                  { PQubits $1 }
    | gate PExp               { PGate $1 $2 }
    | PI PExp                 { PProjector $2 }
    | PExp '*' PExp           { PTimes $1 $3 }
    | '(' PExp ')'            { $2 }
    | letcase var '=' PExp in '{' CaseList '}' { PLetCase $2 $4 (reverse $7) }

CaseList : PExp              { [$1] }
         | CaseList ',' PExp { $3 : $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseLambdaRho :: String -> PExp
parseLambdaRho = parseLambdaRhoTokens.scanTokens

data PExp = PVar String
         | PLambda String PExp
         | PFunApp PExp PExp
         | PQubits String
         | PGate String PExp
         | PProjector PExp
         | PTimes PExp PExp
         | PLetCase String PExp [PExp]
         deriving (Show,Eq)
}
