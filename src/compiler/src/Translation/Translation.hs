module Translation.Translation where
import           Control.Monad.State
import           Data.Complex
import qualified Data.Text                     as T
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Parsing.LamRhoExp
import           Python.PyExp
import           Translation.DensMat
import           Translation.Purification
import           Translation.StateBuilder
import           Typing.GateChecker

translate :: LamRhoExp -> PyExp
translate (PVar v) = PyVar v
translate (PLambda v exp) = PyLambda v (translate exp)
translate (PFunApp exp1 exp2) = PyFunCall (translate exp1) [translate exp2]
translate (PQubits qbits) | qbits == T.replicate n "0" = PyFunCall (PyFun "CompilerCircuit") [PyInt n]
                          | otherwise = translateMatrix m
    where
      n = T.length qbits
      m = HM.toLists $ toDensMatrix $ toVector qbits
translate (PMatrix m) = translateMatrix m
translate (PPair b m) = PyPair (PyInt b) (translate (PMatrix m))
translate (PGateApp gate exp) = evalState (translateGate (translate exp) gate) 0
translate (PProjector n exp) = PyFunCall (PyObjMethod (translate exp) "measure") [PyInt n]
translate (POtimesExp exp1 exp2) = PyFunCall (PyObjMethod (translate exp1) "compose") [translate exp2]
translate (PLetCase v exp exps) = PyFunCall (PyFun "letcase") [translate exp, PyList cases]
  where
    cases = (PyLambda v.translate) <$> exps

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
      gate = PyLambda "c" (PyFunCall (PyFun ("c."<>gateName)) gateArgs)
  modify (+gateSize)
  return $ PyFunCall (PyFun "apply_gate") [gate, pyexp]

translateGateName :: T.Text -> T.Text
translateGateName = T.toLower

translateMatrix :: [[Complex Double]] -> PyExp
translateMatrix m = translate (circuitForState purified)
  where
    purified = purify (HM.fromLists m)
