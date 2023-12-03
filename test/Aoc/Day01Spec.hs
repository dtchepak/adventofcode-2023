{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day01Spec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Aoc.Day01

part1Example :: String
part1Example = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

part2Example :: String
part2Example = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

part2Subset :: String
part2Subset = "2eight8fmktlf\nvjchzt7btthreesix1tcngpbtzsfmvsx\nsjv8\nncqpkzh5twooneoneqfxlqbjjhqsrlkhvdnvtbzpcbj\n449three45three"

getDigits :: String -> [Int]
getDigits s =
  let input = T.lines (T.pack s)
  in (\a -> firstDigit a * 10 + lastDigit a) <$> input

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: example" $
      calibrations part1Example `shouldBe` 142
    it "part 2: firstDigits" $
        firstDigit <$> T.lines (T.pack part2Example) `shouldBe` [2, 8, 1, 2, 4, 1, 7]
    it "part 2: lastDigits" $
        lastDigit  <$> T.lines (T.pack part2Example) `shouldBe` [9, 3, 3, 4, 2, 4, 6]
    it "part 2: digits" $
        getDigits part2Example `shouldBe` [29, 83, 13, 24, 42, 14, 76]
    it "part 2: subset" $
        getDigits part2Subset `shouldBe` [28, 71, 88, 51, 43]
    it "part 2: one digit" $
        getDigits "sjv8" `shouldBe` [88]
    it "part 2: example" $
      calibrations' part2Example `shouldBe` 281
