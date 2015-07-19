module Main where

import Prelude
import Data.List
import Data.Maybe
import Data.Tuple
-- import Test.QuickCheck

-- Main.runLengthEnc $ 1:1:1:2:2:3:Nil
runLengthEnc :: forall a. (Eq a) => List a -> List (Tuple Int a)
runLengthEnc as =
  case head as of
    Just a ->
      let s = span (==a) as
      in Cons (Tuple (length s.init) a) (runLengthEnc s.rest)
    _ -> Nil

-- Main.runLengthDec $ (Tuple 10 2):Nil
runLengthDec :: forall a. List (Tuple Int a) -> List a
runLengthDec = concatMap (uncurry replicate)

-- prop :: Number -> Number -> Result
-- prop x y =
--   let result = x * x + y * y >= x * x
--   in result <?> "x: " ++ show x ++ "; y: " ++ show y

main = do
  -- quickCheck prop
  unit
