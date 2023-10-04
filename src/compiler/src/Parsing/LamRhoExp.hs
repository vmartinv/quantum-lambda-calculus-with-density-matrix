module Parsing.LamRhoExp where

import           Data.Complex
import           Data.Text    (Text)


data PGate = PGate Text [Double] Int
           deriving (Show,Eq)

data LamRhoExp = PVar Text
         | PLambda Text LamRhoExp
         | PFunApp LamRhoExp LamRhoExp
         | PQubits Text
         | PPair Int Int [[Complex Double]]
         | PGateApp PGate LamRhoExp
         | PMatrix [[Complex Double]]
         | PProjector Int LamRhoExp
         | POtimesExp LamRhoExp LamRhoExp
         | PLetCase Text LamRhoExp [LamRhoExp]
         deriving (Show,Eq)
