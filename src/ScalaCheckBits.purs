module ScalaCheckBits (
  suchThat,
  genNumChar,
  genAlphaLowerChar,
  genAlphaUpperChar,
  genAlphaChar,
  genAlphaNumChar
  ) where

import Prelude
import Data.Char
import Data.List
import Data.Tuple
import Test.QuickCheck.Gen

suchThat :: forall a. Gen a -> (a -> Boolean) -> Gen a
suchThat gen p = do
  x <- gen
  if p x then return x else (suchThat gen p)

genCharInRange :: Char -> Char -> Gen Char
genCharInRange l h = fromCharCode `map` chooseInt (toCharCode l) (toCharCode h)

genNumChar :: Gen Char
genNumChar = genCharInRange '0' '9'

genAlphaLowerChar :: Gen Char
genAlphaLowerChar = genCharInRange 'a' 'z'

genAlphaUpperChar :: Gen Char
genAlphaUpperChar = genCharInRange 'A' 'Z'

genAlphaChar :: Gen Char
genAlphaChar = frequency (Tuple 1.0 genAlphaUpperChar) $ singleton (Tuple 9.0 genAlphaLowerChar)

genAlphaNumChar :: Gen Char
genAlphaNumChar = frequency (Tuple 1.0 genNumChar) $ singleton (Tuple 9.0 genAlphaChar)
