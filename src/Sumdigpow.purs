module Codewars.G964.Sumdigpow where

import Data.Char.Unicode (digitToInt)
import Data.Int (pow)
import Data.List.Lazy (filter, iterate, zipWith, (..), List, nil)
import Data.Maybe (fromMaybe)
import Data.String.Yarn (toChars)
import Data.Traversable (sum, traverse)
import Prelude (add, flip, show, ($), (<<<), (==))

-- Take a Number And Sum Its Digits Raised To The Consecutive Powers And ....¡Eureka!!
-- https://www.codewars.com/kata/5626b561280a42ecc50000d1
sumDigPow :: Int -> Int -> List Int
sumDigPow a b = filter f (a..b)
  where
    f n = (_ == n) <<< sum <<< zipWith (flip pow) (iterate (add 1) 1) <<< toDigits $ n

toDigits :: Int -> List Int
toDigits x = fromMaybe nil $ traverse digitToInt $ toChars $ show x
