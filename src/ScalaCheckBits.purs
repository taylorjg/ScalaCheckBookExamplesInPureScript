module ScalaCheckBits (
  suchThat,
  genAlphaLowerChar
  ) where

import Prelude
import Data.Char
import Test.QuickCheck.Gen

suchThat :: forall a. Gen a -> (a -> Boolean) -> Gen a
suchThat gen p = do
  x <- gen
  if p x then return x else (suchThat gen p)

genCharInRange :: Char -> Char -> Gen Char
genCharInRange l h = do
  n <- chooseInt (toCharCode l) (toCharCode h)
  return $ fromCharCode n

genAlphaLowerChar :: Gen Char
genAlphaLowerChar = genCharInRange 'a' 'z'
