module Data.List.ListUtils where

import Prelude

import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (fromMaybe)

consecutives :: forall a. Int -> LL.List a -> LL.List (LL.List a)
consecutives n xs = if (LL.length taken == n)
                    then (taken LL.: (consecutives n $ fromMaybe LL.nil $ LL.tail xs))
                    else LL.nil
  where
    taken = LL.take n xs

-- loopedConsecutives 3 (1 LL... 10)
-- fromStrict ((Cons fromStrict ((Cons 1 (Cons 2 (Cons 3 Nil)))) (Cons fromStrict ((Cons 2 (Cons 3 (Cons 4 Nil)))) (Cons fromStrict ((Cons 3 (Cons 4 (Cons 5 Nil)))) (Cons fromStrict ((Cons 4 (Cons 5 (Cons 6 Nil)))) (Cons fromStrict ((Cons 5 (Cons 6 (Cons 7 Nil)))) (Cons fromStrict ((Cons 6 (Cons 7 (Cons 8 Nil)))) (Cons fromStrict ((Cons 7 (Cons 8 (Cons 9 Nil)))) (Cons fromStrict ((Cons 8 (Cons 9 (Cons 10 Nil)))) (Cons fromStrict ((Cons 9 (Cons 10 (Cons 1 Nil)))) (Cons fromStrict ((Cons 10 (Cons 1 (Cons 2 Nil)))) Nil)))))))))))
loopedConsecutives :: forall a. Int -> LL.List a -> LL.List (LL.List a)
loopedConsecutives n xs = consecutives n $ LL.take (n + LL.length xs - 1) $ LL.cycle xs

-- combination 3 (1..5)
-- ((1 : 2 : 3 : Nil) : (1 : 2 : 4 : Nil) : (1 : 2 : 5 : Nil) : (1 : 3 : 4 : Nil) : (1 : 3 : 5 : Nil) : (1 : 4 : 5 : Nil) : (2 : 3 : 4 : Nil) : (2 : 3 : 5 : Nil) : (2 : 4 : 5 : Nil) : (3 : 4 : 5 : Nil) : Nil)
combination :: forall a. Int -> L.List a -> L.List (L.List a)
combination 0 _ = pure L.Nil
combination _ L.Nil = L.Nil
combination n (L.Cons x xs) = (map (L.Cons x) (combination (n-1) xs)) <> combination n xs
