module Parsing.PExp where


import           Data.Text (Text)


data PGate = PGate Text [Double]
         | PGateOtimes PGate PGate
           deriving (Show,Eq)

data PExp = PVar Text
         | PLambda Text PExp
         | PFunApp PExp PExp
         | PQubits Text
         | PGateApp PGate PExp
         | PMatrix [[Double]]
         | POtimesGate PExp PExp
         | PProjector Int PExp
         | POtimes PExp PExp
         | PLetCase Text PExp [PExp]
         deriving (Show,Eq)
