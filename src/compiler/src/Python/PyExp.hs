module Python.PyExp where

import           Data.Complex
import qualified Data.Text    as T

data PyExp = PyInt Int
          | PyFloat Double
          | PyComplex (Complex Double)
          -- | PyString T.Text
          | PyVar T.Text
          | PyDiv PyExp PyExp
          | PyDiff PyExp PyExp
          | PyPower PyExp PyExp
          | PyPair PyExp PyExp
          | PyLambda T.Text PyExp
          | PyFun T.Text
          | PyObjMethod PyExp T.Text
          | PyFunCall PyExp [PyExp]
          | PyList [PyExp]
          deriving (Show, Eq)
