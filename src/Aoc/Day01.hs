{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day01 (
  calibrations,
  calibrations',
  firstDigit,
  lastDigit,
  day01
) where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Text as T

readInput :: IO String
readInput = readFile "data/day01.txt"

digits :: [Char] -> [Char]
digits = filter isDigit

calibrationLine :: String -> Int
calibrationLine s =
  let d = digits s
  in fromMaybe 0 (readMaybe [headOr '0' d, headOr '0' (reverse d)])

headOr :: Foldable t => a -> t a -> a
headOr = foldr const

digitTokens :: [(T.Text, Int)]
digitTokens =
  [ ("0"    , 0)
  , ("1"    , 1)
  , ("2"    , 2)
  , ("3"    , 3)
  , ("4"    , 4)
  , ("5"    , 5)
  , ("6"    , 6)
  , ("7"    , 7)
  , ("8"    , 8)
  , ("9"    , 9)
  -- "zero" not defined in spec
  , ("one"  , 1)
  , ("two"  , 2)
  , ("three", 3)
  , ("four" , 4)
  , ("five" , 5)
  , ("six"  , 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine" , 9)
  ]

findTokenIndex :: T.Text -> T.Text -> Int
findTokenIndex needle = T.length . fst . T.breakOn needle

compareTokens :: T.Text -> (T.Text, a) -> (T.Text, b) -> Ordering
compareTokens t a b = findTokenIndex (fst a) t `compare` findTokenIndex (fst b) t

firstMatch :: T.Text -> [(T.Text, a)] -> a
firstMatch t = snd . minimumBy (compareTokens t)

firstDigit :: T.Text -> Int
firstDigit t = firstMatch t digitTokens

lastDigit :: T.Text -> Int
lastDigit t = firstMatch (T.reverse t) (first T.reverse <$> digitTokens)

calibrations :: String -> Int
calibrations =
  sum . fmap calibrationLine . lines

calibrations' :: String -> Int
calibrations' =
  sum . fmap (\l -> firstDigit l * 10 + lastDigit l) . T.lines . T.pack

day01 :: IO ()
day01 = do
  input <- readInput
  putStrLn "Part 1:"
  print (calibrations input)
  putStrLn "Part 2:"
  print (calibrations' input)
