module Test.Main where

import Codewars.G964.Sumdigpow

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.List (List(Nil), (:))
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
      Assert.equal (sumDigPow 1 10) (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : Nil)
      Assert.equal (sumDigPow 1 100) (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 89 : Nil)
      Assert.equal (sumDigPow 10 100) (89 : Nil)
      Assert.equal (sumDigPow 90 100) (Nil)
      Assert.equal (sumDigPow 90 150) (135 : Nil)
      Assert.equal (sumDigPow 50 150) (89: 135 : Nil)
      Assert.equal (sumDigPow 10 150) (89: 135 : Nil)
