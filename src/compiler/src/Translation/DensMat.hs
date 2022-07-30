module Translation.DensMat where

import           Data.Complex
import qualified Data.Text                     as T
import qualified Numeric.LinearAlgebra.HMatrix as HM

toDensMatrix :: HM.Vector (Complex Double) -> HM.Matrix (Complex Double)
toDensMatrix v = vm <> HM.conj vm
  where
    vm = HM.asColumn v

toVector :: T.Text -> HM.Vector (Complex Double)
toVector qbits = HM.flatten $ foldr HM.kronecker (HM.ident 1) (bitToVec <$> T.unpack qbits)
  where
    bitToVec :: Char -> HM.Matrix (Complex Double)
    bitToVec '0' = HM.asColumn $ 2 HM.|> [1, 0]
    bitToVec '1' = HM.asColumn $ 2 HM.|> [0, 1]
    bitToVec '+' = HM.asColumn $ 2 HM.|> [1/sqrt 2, 1/sqrt 2]
    bitToVec '-' = HM.asColumn $ 2 HM.|> [1/sqrt 2, -1/sqrt 2]
