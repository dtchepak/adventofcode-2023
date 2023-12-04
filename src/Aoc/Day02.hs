{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day02 (
  Cubes(..),
  Reveal(..),
  Game(..),
  parseGame,
  parseGames,
  runParser,
  part1,
  day02
) where

import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec hiding (State, runParser)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

readInput :: IO T.Text
readInput = T.readFile "data/day02.txt"

type Label = T.Text
data Cubes = Cubes { cubeCount :: Int, cubeLabel :: Label } deriving (Show, Eq)
newtype Reveal = Reveal [Cubes] deriving (Show, Eq)
data Game = Game {
  gameId :: Int,
  reveals :: [Reveal]
} deriving (Show, Eq)

parseLabel :: Parser Label
parseLabel = T.pack <$> many letterChar

parseCubes :: Parser Cubes
parseCubes = Cubes <$> (space *> L.decimal) <*> (space1 *> parseLabel)

parseReveal :: Parser Reveal
parseReveal = Reveal <$> parseCubes `sepBy` char ','

parseGame :: Parser Game
parseGame = do
  gameId <- string "Game " *> L.decimal <* string ": "
  reveals <- parseReveal `sepBy` char ';'
  pure $ Game gameId reveals

parseGames :: Parser [Game]
parseGames = parseGame `sepBy` newline

runParser :: Parser a -> T.Text -> Either String a
runParser p = first show . parse p ""

validGame :: [Cubes] -> Game -> Bool
validGame bag (Game _ r) =
  let allRevealedCubes = r >>= (\(Reveal cubes) -> cubes)
      lookupBag colour = maybe 0 cubeCount (find ((==) colour . cubeLabel) bag)
  in all (\(Cubes i colour) -> i <= lookupBag colour) allRevealedCubes

part1 :: [Game] -> Int
part1 = sum . map gameId . filter (validGame [Cubes 12 "red", Cubes 13 "green", Cubes 14 "blue"])

day02 :: IO ()
day02 = do
  input <- readInput
  let games = runParser parseGames input
  putStrLn "Part 1:"
  print $ part1 <$> games
