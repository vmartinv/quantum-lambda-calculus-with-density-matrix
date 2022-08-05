module Parsing.LamRhoExp where

import           Data.Complex
import           Data.Text    (Text)


data PGate = PGate Text [Double]
         | PGateOtimes PGate PGate
           deriving (Show,Eq)

data LamRhoExp = PVar Text
         | PLambda Text LamRhoExp
         | PFunApp LamRhoExp LamRhoExp
         | PQubits Text
         | PGateApp PGate LamRhoExp
         | PMatrix [[Complex Double]]
         | PProjector Int LamRhoExp
         | POtimesExp LamRhoExp LamRhoExp
         | PLetCase Text LamRhoExp [LamRhoExp]
         deriving (Show,Eq)
