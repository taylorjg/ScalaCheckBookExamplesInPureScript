module Main where

import Prelude
import Data.List
import Data.Maybe
import Data.Tuple
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

runLengthEnc :: forall a. (Eq a) => List a -> List (Tuple Int a)
runLengthEnc as =
  case head as of
    Just a ->
      let s = span (==a) as
      in Cons (Tuple (length s.init) a) (runLengthEnc s.rest)
    _ -> Nil

runLengthDec :: forall a. List (Tuple Int a) -> List a
runLengthDec = concatMap (uncurry replicate)

genOutput :: Gen (List (Tuple Int Char))
-- TODO: just getting the outline of this working - need to implement this properly...
genOutput = return $ (Tuple 5 'a'):Nil

instance arbOutput :: Arbitrary (List (Tuple Int Char)) where
  arbitrary = genOutput

p :: List (Tuple Int Char) -> Boolean
p r = runLengthEnc(runLengthDec(r)) == r

main = do
  quickCheck p
