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
import           Utils

translate :: LamRhoExp -> PyExp
translate = translateSt False

translateSt :: Bool -> LamRhoExp -> PyExp
translateSt _ (PVar v) = PyVar v
translateSt s (PLambda v exp) = dprint "PyLambda" $ PyLambda v (translateSt s exp)
translateSt s (PFunApp exp1 exp2) = PyFunCall (translate exp1) [translateSt s exp2]
translateSt _ (PQubits qbits) | qbits == T.replicate n "0" = PyFunCall (PyFun "Circuit") [PyInt n]
                            | otherwise = translateMatrix m
    where
      n = T.length qbits
      m = HM.toLists $ toDensMatrix $ toVector qbits
translateSt _ (PMatrix m) = translateMatrix m
translateSt s (PPair b m) = PyPair (PyInt b) (translateSt s (PMatrix m))
translateSt s (PGateApp gate exp) = PyFunCall (PyObjMethod (translateSt s exp) gateName) gateArgs
  where
    (gateName, gateArgs) = translateGate s gate
translateSt s (PProjector n exp) = PyFunCall (PyObjMethod (translateSt s exp) "measure") (PyInt <$> [0,2..2*n-1])
translateSt s (POtimesExp exp1 exp2) = PyFunCall (PyObjMethod (translateSt s exp1) "compose") [translateSt s exp2]
translateSt s (PLetCase v exp exps) = PyFunCall (PyFun "letcase") [translateSt s exp, PyList cases]
  where
    cases = (PyLambda "".translateSt s) <$> exps

translateGate :: Bool -> PGate -> (T.Text, [PyExp])
translateGate s gdef@(PGate name params offset) =
  (gateName, gateArgs)
    where
      gateSize = getGateSizeNoCheck gdef
      qubits = if s
        then [offset..offset+gateSize-1]
        else [2*offset,2+2*offset..2*(offset+gateSize)-1]
      gateArgs = (PyFloat <$> params) ++ (PyInt <$> qubits)
      gateName = translateGateName name

translateGateName :: T.Text -> T.Text
translateGateName = T.toLower

translateMatrix :: [[Complex Double]] -> PyExp
translateMatrix m = translateSt True (circuitForState purified)
  where
    purified = dprint "purify" $ purify (HM.fromLists m)
