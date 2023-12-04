{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day02Spec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Aoc.Day02

part1Example :: T.Text
part1Example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
               \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
               \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
               \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
               \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: parse game" $
      runParser parseGame part1Example `shouldBe`
        Right (Game 1 [
                Reveal [Cubes 3 "blue", Cubes 4 "red"],
                Reveal [Cubes 1 "red", Cubes 2 "green", Cubes 6 "blue"],
                Reveal [Cubes 2 "green"]]
        )
    it "part 1: example" $
      runParser (part1 <$> parseGames) part1Example `shouldBe` Right 8
