{-# LANGUAGE TupleSections #-}
module Typing.Robinson where

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
import           Parser
import           Typing.QType
import           Typing.Subst
import           Typing.TypeEq
import           Typing.TypeError
import           Typing.Utils

bind :: VariableId -> QType -> ExceptInfer Subst
bind a t | t == QTVar a     = return emptySubst
       | a `S.member` ftv t = throwError $ InfiniteType a t
       | otherwise       = return $ M.singleton a t

unifies :: QType -> QType -> ExceptInfer Subst
unifies t1 t2                       | t1 == t2 = return emptySubst
unifies (QTVar v) t                 = v `bind` t
unifies t (QTVar v)                 = v `bind` t
unifies (QTFun t1 t2) (QTFun t3 t4) = unifyMany [(t1, t3), (t2, t4)]
unifies t1 t2                       = throwError $ UnificationFail t1 t2

unifyMany :: [(QType, QType)] -> ExceptInfer Subst
unifyMany typeEqs = foldM f emptySubst (reverse typeEqs)
  where f su (t1, t2) = compose su <$> unifies (apply su t1) (apply su t2)

classCheck :: TypeEq -> ExceptInfer ()
classCheck (IsQubits (QTQubits _)) = return ()
classCheck (IsQubits (QTVar _)) = return ()
classCheck (IsQubits t) = throwError $ TypeNotQubits t
classCheck (IsMeasuredQubits (QTMeasuredQubits _)) = return ()
classCheck (IsMeasuredQubits (QTVar _)) = return ()
classCheck (IsMeasuredQubits t) = throwError $ TypeNotMeasuredQubits t
classCheck _ = return ()

robinson :: [TypeEq] -> ExceptInfer Subst
robinson eqs = do
  su1 <- unifyMany [ (t1, t2) | TypeEq t1 t2 <- eqs]
  let eqs' = apply su1 eqs
      sumEqs = dprint "sumEqs" $ [(ls, r) | SumSizeEq ls r <- eqs']
  sequence_ $ classCheck <$> eqs'
  sol <- solveSums eqs'
  su2 <- assignValues eqs' sol
  sequence_ $ classCheck <$> apply su2 eqs'
  return $ su1 `compose` su2

-- given the solution to the sum of equations
-- it creates the corresponding substition based on them
assignValues :: [TypeEq] -> M.Map VariableId Int -> ExceptInfer Subst
assignValues eqs sol = foldr compose emptySubst <$> sequence (bindv <$> M.toList sol)
  where
    measureds = dprint "measureds" $ S.fromList [ v | IsMeasuredQubits (QTVar v) <- eqs]
    cons v | S.member v measureds = QTMeasuredQubits
           | otherwise = QTQubits
    bindv (v, q) = v `bind` cons v q


-- given the sum equations returns a map from variables to their assigned size
solveSums :: [TypeEq] -> ExceptInfer (M.Map VariableId Int)
solveSums eqs = sndPass $ foldr fstPass init_state (reverse eqs)
  where
    set1 = M.fromList ((,1) <$> S.toList (foldr S.union S.empty $ ftv <$> eqs))
    init_state = ([], return set1)
    getVar m (QTVar v)            = maybe (0, [v]) snd (find ((==v).fst) m)
    getVar m (QTQubits q)         = (q, [])
    getVar m (QTMeasuredQubits q) = (q, [])
    combine (q1, xs1) (q2, xs2) = (q1+q2, xs1 ++ xs2)
    combineLst = foldr combine (0, [])
    num (QTQubits q)         = q
    num (QTMeasuredQubits q) = q
    fstPass (SumSizeEq ls (QTVar v)) (vars, su) = (vars', su)
      where
        ls' = combineLst $ getVar vars <$> ls
        vars' = (v, ls'):vars
    fstPass (SumSizeEq ls q) (vars, su) = (vars, su')
      where
        ls' = combineLst $ getVar vars <$> ls
        su' = do
          (v, q) <- solveSingleEq ls' (num q)
          M.insert v q <$> su
    fstPass (AtLeastSizeEq ls q) (vars, su) = (vars, su')
      where
        ls' = combineLst $ getVar vars <$> ls
        su' = do
          (v, q) <- solveSingleIneq ls' (num q)
          M.insert v q <$> su
    fstPass _ (vars, su) = (vars, su)

    sndPass (vars, su) = fold <$> su
      where
        fold su = (foldr f su (reverse vars))::M.Map VariableId Int
        f (v, (q, vs)) su = M.insert v (q'::Int) su
          where
            q' = q + (foldr (+) 0 ((flip (M.findWithDefault 1)) su <$> vs))

-- finds v_1,...v_n / v_1+...+v_n+q_f>=q_t, if it exists
-- by fixing v_2,..., v_n to 1 and returning (v_1, value)
solveSingleIneq :: (Int, [VariableId]) -> Int -> ExceptInfer (VariableId, Int)
solveSingleIneq (qf, vs) qt | qf < qt && null vs = throwError InvalidOperatorSizes
                        | null vs = return (-1, 0) --solution no vars
                        | otherwise = return sol
  where
    v = head vs
    sol = (v, max 1 (qt - qf - length vs + 1))

-- finds v_1,...v_n / v_1+...+v_n+q_f=q_t, if it exists
-- by fixing v_2,..., v_n to 1 and returning (v_1, value)
solveSingleEq :: (Int, [VariableId]) -> Int -> ExceptInfer (VariableId, Int)
solveSingleEq (qf, vs) qt | qf + length vs > qt = throwError InvalidOperatorSizes
                        | qf /= qt && null vs = throwError InvalidOperatorSizes
                        | otherwise = solveSingleIneq (qf, vs) qt
