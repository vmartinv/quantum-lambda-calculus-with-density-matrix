module Parsing.PExp where

import           Data.Complex
import           Data.Text    (Text)


data PGate = PGate Text [Double]
         | PGateOtimes PGate PGate
           deriving (Show,Eq)

data PExp = PVar Text
         | PLambda Text PExp
         | PFunApp PExp PExp
         | PQubits Text
         | PGateApp PGate PExp
         | PMatrix [[Complex Double]]
         | PProjector Int PExp
         | POtimesExp PExp PExp
         | PLetCase Text PExp [PExp]
         deriving (Show,Eq)
