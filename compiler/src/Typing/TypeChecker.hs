{-# LANGUAGE TupleSections #-}
module Typing.TypeChecker where

import           CompilerError
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
import           Parsing.LamRhoExp
import           Typing.Hindley
import           Typing.QType
import           Typing.Robinson
import           Typing.Subst
import           Typing.TypeEq
import           Utils

-- | Solve for the toplevel type of an expression
typeCheck :: LamRhoExp -> ExceptInfer QType
typeCheck = fullTypeCheck (TypeEnv M.empty)

fullTypeCheck :: TypeEnv -> LamRhoExp -> ExceptInfer QType
fullTypeCheck env ex = do
  (t, eqs) <- runHindley env ex
  subst <- robinson eqs
  closeOver (dprint "robinsonApplied" $ apply subst t)

closeOver :: QType -> ExceptInfer QType
closeOver t = foldr apply t <$> mapM assign1 (S.toList (ftv t))
  where
    assign1 v = v `bind` (QTQubits 1)
