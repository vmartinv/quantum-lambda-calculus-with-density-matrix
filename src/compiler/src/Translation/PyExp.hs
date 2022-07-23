module Translation.PyExp where

import           Data.Text (Text)

data PyExp = PyVar Text
          | PyLambda Text PyExp
          | PyFunName Text
          | PyFunCall PyExp [PyExp]
          | PyStr Text
          | PyFloat Double
          | PyInt Int
          | PyDict [(Text, PyExp)]
          deriving (Show,Eq)
