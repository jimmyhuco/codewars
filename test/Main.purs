module Test.Main where

import Codewars.G964.Sumdigpow
import Data.List.Lazy (nil, (:))

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude (Unit, discard)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main ::
  forall eff .
  Eff ( console :: CONSOLE
      , testOutput :: TESTOUTPUT
      , avar :: AVAR
      | eff) Unit
main = runTest do
  suite "sync code" do
    test "sumDigPow" do
      Assert.equal (sumDigPow 1 10) (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : nil)
      Assert.equal (sumDigPow 1 100) (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 89 : nil)
      Assert.equal (sumDigPow 10 100) (89 : nil)
      Assert.equal (sumDigPow 90 100) (nil)
      Assert.equal (sumDigPow 90 150) (135 : nil)
      Assert.equal (sumDigPow 50 150) (89: 135 : nil)
      Assert.equal (sumDigPow 10 150) (89: 135 : nil)
