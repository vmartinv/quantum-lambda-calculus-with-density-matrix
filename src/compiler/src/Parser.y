{
module Parser where
import Lexer
import Control.Monad.Except

import Data.Text (Text, pack)
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
    OTIMES { TokenTimes }
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
%left OTIMES ','

%%

PExp : var                    { PVar (pack $1) }
    | PI PExp                 { PProjector $2 }
    | '\\' var '.' PExp       { PLambda (pack $2) $4 }
    | PExp PExp               { PFunApp $1 $2 }
    | qubits                  { PQubits (pack $1) }
    | gate PExp               { PGate (pack $1) $2 }
    | PExp OTIMES PExp        { PTimes $1 $3 }
    | '(' PExp ')'            { $2 }
    | letcase var '=' PExp in '{' CaseList '}' { PLetCase (pack $2) $4 (reverse $7) }

CaseList : PExp              { [$1] }
         | CaseList ',' PExp { $3 : $1 }

{
data PExp = PVar Text
         | PLambda Text PExp
         | PFunApp PExp PExp
         | PQubits Text
         | PGate Text PExp
         | PProjector PExp
         | PTimes PExp PExp
         | PLetCase Text PExp [PExp]
         deriving (Show,Eq)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseLambdaRho :: String -> Except String PExp
parseLambdaRho =  scanTokens >=> parseTokens

}
