{-# LANGUAGE FlexibleContexts #-}
module TestUtils where
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Test.Tasty
import           Test.Tasty.HUnit

approxEqualA :: [Complex Double] -> [Complex Double] -> Assertion
approxEqualA v1 v2 = approxEqualVA (HM.fromList v1) (HM.fromList v2)

approxEqualVA :: Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> Assertion
approxEqualVA v1 v2 = HM.norm_2 (v1 - v2) `compare` 1e5 @?= LT

approxEqualV :: Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> Bool
approxEqualV v1 v2 = HM.norm_2 (v1 - v2) `compare` 1e5 == LT
