module Aoc.Day01 (
  calibrations,
  answers
) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

readInput :: IO Int
readInput = calibrations <$> readFile "data/day01.txt"

digits :: [Char] -> [Char]
digits = filter isDigit

calibrationLine :: String -> Int
calibrationLine s =
  let d = digits s
  in fromMaybe 0 (readMaybe [headOr '0' d, headOr '0' (reverse d)])

calibrations :: String -> Int
calibrations =
  sum . fmap calibrationLine . lines

headOr :: Foldable t => a -> t a -> a
headOr = foldr const

answers :: IO ()
answers = do
  input <- readInput
  putStrLn "Part 1:"
  print input
