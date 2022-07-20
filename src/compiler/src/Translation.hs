module Translation where
import           Control.Monad.State
import           Data.Text           (Text, pack, toLower)
import           PyExp
import           Typing.GateCheck
import           Typing.PExp

translate :: PExp -> PyExp
translate (PVar v) = PyVar v
translate (PLambda v exp) = PyLambda v (translate exp)
translate (PFunApp exp1 exp2) = PyFunCall (translate exp1) [translate exp2]
translate (PQubits qbits) = PyFunCall (PyFunName "make_pure") [PyStr qbits]
translate (PGate gdefs exp) = evalState (foldM translateGateDef (translate exp) gdefs) 0
translate (PProjector _ exp) = PyFunCall (PyFunName "apply_measure") [translate exp]
translate (POtimes exp1 exp2) = PyFunCall (PyFunName "tensor_product") [translate exp1, translate exp2]
translate (PLetCase v exp exps) = PyFunCall (PyFunName "letcase") [translate exp, PyDict cases]
  where
    cases = zip (pack <$> show <$> [0::Int ..]) ((PyLambda v.translate) <$> exps)


translateGateDef :: PyExp -> PGateDef -> State Int PyExp
translateGateDef pyexp gdef@(PGateDef "I" params) = do
  modify (+getGateDefSizeNoCheck gdef)
  return pyexp
translateGateDef pyexp gdef@(PGateDef name params) = do
  offset <- get
  let gateSize = getGateDefSizeNoCheck gdef
      gateArgs = (PyFloat <$> params) ++ (PyInt <$> [offset..offset+gateSize])
      gateName = translateGateName name
      gate = PyLambda "c" (PyFunCall (PyFunName ("c."<>gateName)) gateArgs)
  modify (+gateSize)
  return $ PyFunCall (PyFunName "apply_gate") [gate, pyexp]

translateGateName :: Text -> Text
translateGateName = toLower
