module Typing.Subst where

import qualified Data.Map      as M
import qualified Data.Set      as S
import           Typing.QType
import           Typing.TypeEq
import           Typing.Utils

type Subst = M.Map VariableId QType

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set VariableId

instance Substitutable QType where
  apply _ t@(QTQubits _)         = t
  apply _ t@(QTMeasuredQubits _) = t
  apply s t@(QTVar a)            = M.findWithDefault t a s
  apply s (t1 `QTFun` t2)        = apply s t1 `QTFun` apply s t2

  ftv (QTVar v)     = S.singleton v
  ftv (QTFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv _             = S.empty

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable TypeEq where
  apply s (SumSizeEq ts tr)    = SumSizeEq (map (apply s) ts) (apply s tr)
  apply s (AtLeastSizeEq ts tr)    = AtLeastSizeEq (map (apply s) ts) (apply s tr)
  apply s (IsQubits t)         = IsQubits $ apply s t
  apply s (IsMeasuredQubits t) = IsMeasuredQubits $ apply s t
  apply s (TypeEq t1 t2)       = TypeEq (apply s t1) (apply s t2)
  ftv (SumSizeEq ts tr)     = ftv (tr:ts)
  ftv (AtLeastSizeEq ts tr) = ftv (tr:ts)
  ftv (IsQubits t)          = ftv t
  ftv (IsMeasuredQubits t)  = ftv t
  ftv (TypeEq t1 t2)        = ftv [t1, t2]

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1
