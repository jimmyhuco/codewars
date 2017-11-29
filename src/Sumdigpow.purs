module Codewars.G964.Sumdigpow where

import Data.Maybe (fromMaybe)
import Data.Char.Unicode (digitToInt)
import Data.Int (pow)
import Data.List (List(Nil), (..), filter, (:))
import Data.String.Yarn (toChars)
import Data.Traversable (traverse)
import Prelude (show, ($), (+), (==))

-- Take a Number And Sum Its Digits Raised To The Consecutive Powers And ....¡Eureka!!
-- https://www.codewars.com/kata/5626b561280a42ecc50000d1
sumDigPow :: Int -> Int -> List Int
sumDigPow a b = filter sdp (a..b)

sdp :: Int -> Boolean
sdp x = x == sdp' (toDigits x) 1
  where
    sdp' :: List Int -> Int -> Int
    sdp' Nil count = 0
    sdp' (x:xs) count = pow x count + sdp' xs (count+1)

toDigits :: Int -> List Int
toDigits x = fromMaybe Nil $ traverse digitToInt $ toChars $ show x
