module QType where
import           Prettyprinter

data QType = QTQubits Int
        | QTMeasuredQubits Int
        | QTFun QType QType
        deriving (Eq, Ord)

instance Show QType where
  show = show.(enclose "$" "$").  prettyQType

prettyQType (QTQubits n) = parens $ pretty n
prettyQType (QTMeasuredQubits n) = parens $ sep $ punctuate comma (pretty <$> [n, n])
prettyQType (QTFun f x) = smartParen f <+> "\\multimap" <+> smartParen x
  where smartParen f@(QTQubits _)         = prettyQType f
        smartParen f@(QTMeasuredQubits _) = prettyQType f
        smartParen f@(QTFun _ _)          = parens $ prettyQType f
