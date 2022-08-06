module Python.PyExp where

import qualified Data.Text as T

data PyExp = PyInt Int
          | PyFloat Double
          | PyVar T.Text
          | PyPair PyExp PyExp
          | PyLambda T.Text PyExp
          | PyFun T.Text
          | PyObjMethod PyExp T.Text
          | PyFunCall PyExp [PyExp]
          | PyList [PyExp]
          deriving (Show,Eq)
