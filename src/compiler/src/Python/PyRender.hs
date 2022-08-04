module Python.PyRender where

import           Data.List
import           Prettyprinter
import           Python.PyExp

pyRender :: PyExp -> Doc ann
pyRender (PyVar v)            = pretty v
pyRender (PyLambda v exp)     = "lambda" <+> pretty v <> colon <+> pyRender exp
pyRender( PyFunCall exp1 exps) = callee exp1 <> parens (args exps)
    where
      callee (PyFun name)             = pretty name
      callee (PyObjMethod obj method) = pyRender obj <> dot <> pretty method
      callee exp                      = parens $ pyRender exp
      args [x] = pyRender x
      args xs  = enclose line line $ indent 4 $ vsep $ punctuate comma $ (pyRender <$> xs)
pyRender (PyInt n) = pretty n
pyRender (PyFloat f) = pretty f
pyRender (PyList cases) = brackets $ enclose line line $ indent 4 $ sep $ (prettyCase <$> cases)
  where
    prettyCase exp = pyRender exp <> comma
