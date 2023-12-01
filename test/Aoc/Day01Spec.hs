module Aoc.Day01Spec (spec) where

import Test.Hspec
import Aoc.Day01

part1Example :: String
part1Example = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: example" $
      calibrations part1Example `shouldBe` 142
