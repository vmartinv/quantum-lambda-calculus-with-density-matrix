module Translation where
import           Data.Text (Text, pack)
import           Parser

data PyExp = PyVar Text
          | PyLambda Text PyExp
          | PyFunName Text
          | PyFunCall PyExp [PyExp]
          | PyStr Text
          | PyDict [(Text, PyExp)]
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
    cases = zip (pack <$> show <$> [0::Int ..]) ((PyLambda v.translate) <$> exps)
