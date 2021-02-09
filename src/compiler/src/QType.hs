module QType where
import           Data.Text     (Text)
import           Prettyprinter

data QType = QTQubits Int
        | QTMeasuredQubits Int
        | QTFun QType QType
        | QTVar Text
        deriving (Eq, Ord)

data Scheme = Forall [Text] QType
        deriving (Eq, Ord)

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

instance Show Scheme where
  show = show.mathEnv.prettyScheme

prettyScheme (Forall [] t) = prettyQType t
prettyScheme (Forall vs t) = "\\forall" <+> hsep (pretty <$> vs) <> dot <> prettyQType t
