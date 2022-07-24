module Python.Render where

import           Data.List
import           Prettyprinter
import           Python.PyExp

render :: PyExp -> Doc ann
render (PyVar v)            = pretty v
render (PyLambda v exp)     = "lambda" <+> pretty v <> colon <+> render exp
render (PyFunCall exp1 exps) = callee exp1 <> parens (args exps)
    where
      callee (PyFunName name) = pretty name
      callee exp              = parens $ render exp
      args [x@(PyStr s)] = render x
      args xs  = enclose line line $ indent 4 $ vsep $ punctuate comma $ (render <$> xs)
render (PyStr s) = squotes $ pretty s
render (PyInt n) = pretty n
render (PyFloat f) = pretty f
render (PyDict cases) = braces $ enclose line line $ indent 4 $ sep $ (prettyCase <$> cases)
  where
    prettyCase (label, exp) = render (PyStr label) <> colon <+> render exp <> comma
