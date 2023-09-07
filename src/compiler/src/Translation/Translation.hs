module Translation.Translation where
import           Control.Monad.State
import           Data.Complex
import qualified Data.Text                     as T
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Parsing.LamRhoExp
import           Python.PyExp
import           Translation.DensMat
import           Translation.Purification
import           Typing.GateChecker
import           Utils

translate :: LamRhoExp -> PyExp
translate (PVar v) = PyVar v
translate (PLambda v exp) = dprint "PyLambda" $ PyLambda v (translate exp)
translate (PFunApp exp1 exp2) = PyFunCall (translate exp1) [translate exp2]
translate (PQubits qbits) = translateMatrix m
    where
      m = toDensMatrix $ toVector qbits
translate (PMatrix m) = translateMatrix (HM.fromLists m)
translate (PPair b m) = PyPair (PyInt b) (translate (PMatrix m))
translate (PGateApp gate exp) = PyFunCall (PyObjMethod (translate exp) gateName) gateArgs
  where
    (gateName, gateArgs) = translateGate gate
translate (PProjector n exp) = PyFunCall (PyObjMethod (translate exp) "measure") (PyInt <$> [0,2..2*n-1])
translate (POtimesExp exp1 exp2) = PyFunCall (PyObjMethod (translate exp1) "compose") [translate exp2]
translate (PLetCase v exp exps) = PyFunCall (PyFun "letcase") [translate exp, PyList cases]
  where
    cases = (PyLambda "".translate) <$> exps

translateGate :: PGate -> (T.Text, [PyExp])
translateGate gdef@(PGate name params offset) =
  (gateName, gateArgs)
    where
      gateSize = getGateSizeNoCheck gdef
      qubits = [2*offset,2+2*offset..2*(offset+gateSize)-1]
      gateArgs = (PyFloat <$> params) ++ (PyInt <$> qubits)
      gateName = translateGateName name

translateGateName :: T.Text -> T.Text
translateGateName = T.toLower

translateMatrix :: HM.Matrix (Complex Double) -> PyExp
translateMatrix m = PyFunCall (PyFun "Circuit") [stateExp]
  where
    purified = dprint "purify" $ HM.toList $ purify m
    stateExp = PyList (PyComplex <$> purified)
