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

Exp : var                    { Var $1 }
    | '\\' var '.' Exp       { Lambda $2 $4 }
    | Exp Exp                { FunApp $1 $2 }
    | qubits                 { Qubits $1 }
    | gate Exp               { Gate $1 $2 }
    | PI Exp                 { Projector $2 }
    | Exp '*' Exp            { Times $1 $3 }
    | '(' Exp ')'            { $2 }
    | letcase var '=' Exp in '{' CaseList '}' { LetCase $2 $4 (reverse $7) }

CaseList : Exp              { [$1] }
         | CaseList ',' Exp { $3 : $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseLambdaRho :: String -> Exp
parseLambdaRho = parseLambdaRhoTokens.scanTokens

data Exp = Var String
         | Lambda String Exp
         | FunApp Exp Exp
         | Qubits String
         | Gate String Exp
         | Projector Exp
         | Times Exp Exp
         | LetCase String Exp [Exp]
         deriving (Show,Eq)
}
