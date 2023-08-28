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
translate (PGateApp gate exp) = PyFunCall (PyFun "apply_gate") [translateGate gate, translate exp]
translate (PProjector n exp) = PyFunCall (PyObjMethod (translate exp) "measure") [PyInt n]
translate (POtimesExp exp1 exp2) = PyFunCall (PyObjMethod (translate exp1) "compose") [translate exp2]
translate (PLetCase v exp exps) = PyFunCall (PyFun "letcase") [translate exp, PyList cases]
  where
    cases = (PyLambda v.translate) <$> exps

translateGate :: PGate -> PyExp
translateGate gdef@(PGate name params offset) =
  PyLambda "c" (PyFunCall (PyFun ("c."<>gateName)) gateArgs)
    where
      gateSize = getGateSizeNoCheck gdef
      gateArgs = (PyFloat <$> params) ++ (PyInt <$> [offset..offset+gateSize])
      gateName = translateGateName name

translateGateName :: T.Text -> T.Text
translateGateName = T.toLower

translateMatrix :: [[Complex Double]] -> PyExp
translateMatrix m = translate (circuitForState purified)
  where
    purified = purify (HM.fromLists m)
