module Typing.PExp where


import           Data.Text (Text)

data PExp = PVar Text
         | PLambda Text PExp
         | PFunApp PExp PExp
         | PQubits Text
         | PGate Text [Double] PExp
         | PProjector Int PExp
         | POtimes PExp PExp
         | PLetCase Text PExp [PExp]
         deriving (Show,Eq)
