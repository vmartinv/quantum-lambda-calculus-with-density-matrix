module Parsing.PExp where


import           Data.Text (Text)


data PGateDef = PGateDef Text [Double]
  deriving (Show,Eq)

data PExp = PVar Text
         | PLambda Text PExp
         | PFunApp PExp PExp
         | PQubits Text
         | PGate [PGateDef] PExp
         | POtimesGate PExp PExp
         | PProjector Int PExp
         | POtimes PExp PExp
         | PLetCase Text PExp [PExp]
         deriving (Show,Eq)
