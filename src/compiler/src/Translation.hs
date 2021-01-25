module Translation where

import           Grammar

data PyExp = PyVar String
          | PyLambda String PyExp
          | PyFunName String
          | PyFunCall PyExp [PyExp]
          | PyStr String
          | PyDict [(String, PyExp)]
          deriving (Show,Eq)

translate :: PExp -> PyExp
translate (PVar v) = PyVar v
translate (PLambda v exp) = PyLambda v (translate exp)
translate (PFunApp exp1 exp2) = PyFunCall (translate exp1) [translate exp2]
translate (PQubits qbits) = PyFunCall (PyFunName "make_pure") [PyStr qbits]
translate (PGate g exp) = PyFunCall (PyFunName "apply_gate") [PyStr g, translate exp]
translate (PProjector exp) = PyFunCall (PyFunName "apply_measure") [translate exp]
translate (PTimes exp1 exp2) = PyFunCall (PyFunName "tensor_product") [translate exp1, translate exp2]
translate (PLetCase v exp exps) = PyFunCall (PyFunName "letcase") [translate exp, PyDict cases]
  where
    cases = zip (show <$> [0::Int ..]) (translate <$> exps)
