{-# LANGUAGE FlexibleContexts #-}
module TestUtils where
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Test.Tasty
import           Test.Tasty.HUnit
import Foreign.Storable

approxEqualA :: [Complex Double] -> [Complex Double] -> Assertion
approxEqualA v1 v2 = approxEqualVA (HM.fromList v1) (HM.fromList v2)

approxEqualVA :: Storable a => Eq a => Show a=> Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> Assertion
approxEqualVA v1 v2 = assertBool errorMsg $ approxEqualV v1 v2
  where
    errorMsg = "expected: "<>show v2<>"\n but got: "<>show v1<>"\n difference: "<>show (HM.norm_2 (v1 - v2))

approxEqualV :: Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> Bool
approxEqualV v1 v2 = HM.norm_2 (v1 - v2) < 1e-5
