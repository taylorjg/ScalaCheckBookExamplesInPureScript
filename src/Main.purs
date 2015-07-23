module Main where

import Prelude
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Char
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

genOutput :: Gen (List (Tuple Int Char))
genOutput =
  sized rleList
  where
    rleItem :: Gen (Tuple Int Char)
    rleItem = do
      n <- chooseInt 1 20
      c <- genAlphaLowerChar
      return $ Tuple n c
    rleList :: Int -> Gen (List (Tuple Int Char))
    rleList size =
      if size <= 1 then do
        t <- rleItem
        return $ singleton t
      else do
        tl <- rleList (size - 1)
        case head tl of
          Just (Tuple _ c1) -> do
            hd <- suchThat rleItem $ \(Tuple _ c2) -> c1 /= c2
            return $ hd:tl
          _ -> return Nil

instance arbOutput :: Arbitrary (List (Tuple Int Char)) where
  arbitrary = genOutput

p :: List (Tuple Int Char) -> Boolean
p r = runLengthEnc(runLengthDec(r)) == r

main = do
  quickCheck p
