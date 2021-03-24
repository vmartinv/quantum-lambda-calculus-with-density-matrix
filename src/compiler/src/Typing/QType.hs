module Typing.QType where
import           Data.Text     (Text)
import           Prettyprinter

type VariableId = Int

data QType = QTQubits Int
        | QTMeasuredQubits Int
        | QTFun QType QType
        | QTVar VariableId
        deriving (Eq)

instance Show QType where
  show = show.mathEnv.prettyQType

mathEnv = enclose "$" "$"

prettyQType (QTQubits n) = parens $ pretty n
prettyQType (QTVar v) = pretty v
prettyQType (QTMeasuredQubits n) = parens $ sep $ punctuate comma (pretty <$> [n, n])
prettyQType (QTFun f x) = smartParen f <+> "\\multimap" <+> smartParen x
  where smartParen f@(QTQubits _)         = prettyQType f
        smartParen f@(QTMeasuredQubits _) = prettyQType f
        smartParen f@(QTVar _)            = prettyQType f
        smartParen f@(QTFun _ _)          = parens $ prettyQType f
