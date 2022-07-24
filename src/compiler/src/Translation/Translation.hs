module Translation.Translation where
import           Control.Monad.State
import qualified Data.Text                as T
import           Parsing.PExp
import           Python.PyExp
import           Translation.Purification
import           Translation.StateBuilder
import           Typing.GateChecker

translate :: PExp -> PyExp
translate (PVar v) = PyVar v
translate (PLambda v exp) = PyLambda v (translate exp)
translate (PFunApp exp1 exp2) = PyFunCall (translate exp1) [translate exp2]
translate (PQubits qbits) = PyFunCall (PyFunName "make_pure") [PyStr qbits]
translate (PMatrix m) = translateMatrix m
translate (PGateApp gate exp) = evalState (translateGate (translate exp) gate) 0
translate (PProjector _ exp) = PyFunCall (PyFunName "apply_measure") [translate exp]
translate (POtimes exp1 exp2) = PyFunCall (PyFunName "tensor_product") [translate exp1, translate exp2]
translate (PLetCase v exp exps) = PyFunCall (PyFunName "letcase") [translate exp, PyDict cases]
  where
    cases = zip (T.pack <$> show <$> [0::Int ..]) ((PyLambda v.translate) <$> exps)

translateGate :: PyExp -> PGate -> State Int PyExp
translateGate pyexp (PGateOtimes g1 g2) = do
  pyexp' <- translateGate pyexp g1
  translateGate pyexp' g2
translateGate pyexp gdef@(PGate "I" params) = do
  modify (+getGateSizeNoCheck gdef)
  return pyexp
translateGate pyexp gdef@(PGate name params) = do
  offset <- get
  let gateSize = getGateSizeNoCheck gdef
      gateArgs = (PyFloat <$> params) ++ (PyInt <$> [offset..offset+gateSize])
      gateName = translateGateName name
      gate = PyLambda "c" (PyFunCall (PyFunName ("c."<>gateName)) gateArgs)
  modify (+gateSize)
  return $ PyFunCall (PyFunName "apply_gate") [gate, pyexp]

translateGateName :: T.Text -> T.Text
translateGateName = T.toLower

translateMatrix :: [[Double]] -> PyExp
translateMatrix m = translate (circuitForState purified)
  where
    purified = purify m
