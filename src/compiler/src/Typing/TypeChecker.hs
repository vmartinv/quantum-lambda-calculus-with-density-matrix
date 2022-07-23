{-# LANGUAGE TupleSections #-}
module Typing.TypeChecker where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Coerce
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.Tuple.Extra
import           Parsing.PExp
import           Typing.Hidley
import           Typing.QType
import           Typing.Robinson
import           Typing.Subst
import           Typing.TypeEq
import           Typing.TypeError

-- | Solve for the toplevel type of an expression
typeCheck :: PExp -> Except String QType
typeCheck = (withExcept show).(fullTypeCheck (TypeEnv M.empty))

fullTypeCheck :: TypeEnv -> PExp -> ExceptInfer QType
fullTypeCheck env ex = do
  (t, eqs) <- runHidley env ex
  subst <- robinson eqs
  closeOver (apply subst t)

closeOver :: QType -> ExceptInfer QType
closeOver t = foldr apply t <$> mapM assign1 (S.toList (ftv t))
  where
    assign1 v = v `bind` (QTQubits 1)
