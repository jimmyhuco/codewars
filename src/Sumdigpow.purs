module Codewars.G964.Sumdigpow where

import Data.Char.Unicode (digitToInt)
import Data.Int (pow)
import Data.List (List(Nil), filter, zipWith, (..), (:))
import Data.Maybe (fromMaybe)
import Data.String.Yarn (toChars)
import Data.Traversable (sum, traverse)
import Prelude (flip, show, ($), (+), (<<<), (==))

-- Take a Number And Sum Its Digits Raised To The Consecutive Powers And ....¡Eureka!!
-- https://www.codewars.com/kata/5626b561280a42ecc50000d1
sumDigPow' :: Int -> Int -> List Int
sumDigPow' a b = filter sdp (a..b)

sumDigPow :: Int -> Int -> List Int
sumDigPow a b = filter f (a..b)
  where
    f n = (_ == n) <<< sum <<< zipWith (flip pow) (1..100) <<< toDigits $ n

sdp :: Int -> Boolean
sdp x = x == sdp' (toDigits x) 1
  where
    sdp' :: List Int -> Int -> Int
    sdp' Nil count = 0
    sdp' (y:ys) count = pow y count + sdp' ys (count+1)

toDigits :: Int -> List Int
toDigits x = fromMaybe Nil $ traverse digitToInt $ toChars $ show x
