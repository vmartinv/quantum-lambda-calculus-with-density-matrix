module Typing.QType where
import qualified Data.Map      as M
import qualified Data.Text     as T
import           Prettyprinter

type VariableId = Int

data QType = QTQubits Int
        | QTMeasuredQubits Int QType
        | QTFun QType QType
        | QTVar VariableId
        deriving (Eq)

instance Show QType where
  show = show.mathEnv.prettyQType

newtype TypeEnv = TypeEnv (M.Map T.Text QType)

mathEnv = enclose "$" "$"

prettyQType (QTQubits n) = parens $ pretty n
prettyQType (QTVar v) = "V" <> pretty v
prettyQType (QTMeasuredQubits n (QTQubits m)) = parens $ sep $ punctuate comma (pretty <$> [n, m])
prettyQType (QTMeasuredQubits n v) = parens $ sep $ punctuate comma [pretty n, prettyQType v]
prettyQType (QTFun f x) = smartParen f <+> "->" <+> smartParen x
  where smartParen f@(QTQubits _)           = prettyQType f
        smartParen f@(QTMeasuredQubits _ _) = prettyQType f
        smartParen f@(QTVar _)              = prettyQType f
        smartParen f@(QTFun _ _)            = parens $ prettyQType f
