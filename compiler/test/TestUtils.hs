{-# LANGUAGE FlexibleContexts #-}
module TestUtils where
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Test.Tasty
import qualified  Data.Map as M
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

approxEqualV :: Storable a => Eq a => Show a => (HM.Container HM.Vector a) => Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> QCP.Result
approxEqualV v1 v2 = if  approxEqualVB v1 v2 then QCP.succeeded else QCP.failed { QCP.reason = errorMsg }
  where
    errorMsg = "expected: "<>show v2<>"\n but got: "<>show v1<>"\n difference: "<>show (HM.norm_2 (v1 - v2))

approxEqualVBEps :: HM.Vector Double -> HM.Vector Double -> Double -> Bool
approxEqualVBEps v1 v2 eps = HM.size v1 == HM.size v2 && HM.norm_2 (v1 - v2) < eps

approxEqualVB :: (HM.Container HM.Vector a) => Num (HM.Vector a) => HM.Normed (HM.Vector a) => HM.Vector a -> HM.Vector a -> Bool
approxEqualVB v1 v2 = HM.size v1 == HM.size v2 && HM.norm_2 (v1 - v2) < eps

matchesRegex :: T.Text -> T.Text -> QCP.Result
matchesRegex reg out =
    if match then QCP.succeeded else QCP.failed { QCP.reason = T.unpack errorMsg }
  where
    match = out =~ reg
    errorMsg = "Output doesn't match regex.\nOutput:\n"<>out<>"\nRegex:\n"<>reg

isClassicMeasurement :: Int -> String -> Assertion
isClassicMeasurement val out = st $
    matchesRegex regex (T.pack out)
  where
    regex = T.pack $ "\\(\\("<>show val<>", [0-9]+\\), <preamble.Circuit object at 0x[0-9a-f]+>\\)\n"

removeWhiteSpaces :: T.Text -> T.Text
removeWhiteSpaces =  T.replace "    " "" . T.replace "\n" ""

approxEqualMeasurement :: M.Map Int Double -> M.Map Int Double -> QCP.Result
approxEqualMeasurement b a =
    if M.size a == M.size b && M.keys a == M.keys b && approxEqualVBEps (HM.fromList (M.elems a)) (HM.fromList (M.elems b)) 0.07
      then QCP.succeeded
      else QCP.failed { QCP.reason = errorMsg }
  where
    errorMsg = "expected: "<>show b<>"\n but got: "<>show a
