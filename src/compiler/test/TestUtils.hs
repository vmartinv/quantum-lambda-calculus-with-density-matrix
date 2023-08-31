{-# LANGUAGE FlexibleContexts #-}
module TestUtils where
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified          Test.Tasty.QuickCheck         as QC
import qualified           Test.Tasty.SmallCheck         as SC
import Foreign.Storable
import Test.QuickCheck.Property
import Data.Maybe
import qualified Test.QuickCheck.Property      as QCP
import qualified Test.SmallCheck               as SCP
import           Text.Regex.TDFA
import Data.Text as T

precision :: Int
precision = 6

eps :: Double
eps = 10**(-fromIntegral precision)

roundDec :: Double -> Double
roundDec num = (fromIntegral . round $ num * f) / f
  where f = 10^precision

st :: QCP.Result -> Assertion
st res = assertBool (QCP.reason res) $ fromMaybe True (QCP.ok res)

qct :: QCP.Result -> QCP.Result
qct = id

sct :: QCP.Result -> Either SCP.Reason SCP.Reason
sct res = if fromMaybe True (QCP.ok res) then Right "ok" else Left (QCP.reason res)

tequal :: Eq a => Show a => a -> a -> QCP.Result
tequal a b = if a==b then QCP.succeeded else QCP.failed { QCP.reason = errorMsg }
  where
    errorMsg = "expected: "<>show b<>"\n but got: "<>show a

approxEqualV :: Storable a => Eq a => Show a=> Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> QCP.Result
approxEqualV v1 v2 = if  approxEqualVB v1 v2 then QCP.succeeded else QCP.failed { QCP.reason = errorMsg }
  where
    errorMsg = "expected: "<>show v2<>"\n but got: "<>show v1<>"\n difference: "<>show (HM.norm_2 (v1 - v2))

approxEqualVB :: Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> Bool
approxEqualVB v1 v2 = HM.norm_2 (v1 - v2) < eps

matchesRegex :: T.Text -> T.Text -> QCP.Result
matchesRegex reg out =
    if match then QCP.succeeded else QCP.failed { QCP.reason = T.unpack errorMsg }
  where
    match = out =~ reg
    errorMsg = "Output doesn't match regex.\nOutput:\n"<>out<>"\nRegex:\n"<>reg

removeWhiteSpaces :: T.Text -> T.Text
removeWhiteSpaces =  T.replace "    " "" . T.replace "\n" ""
