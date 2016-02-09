module Main where

import Data.List
import Data.Char
import qualified Control.Exception.Base as Exception
import Data.List.Split
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Player
import Field

constructField :: String -> Field
constructField image =
  let width = length $ takeWhile (/= ' ') $ dropWhile (== ' ') image
      lines = chunksOf width $ filter (/= ' ') image
      assertion = Exception.assert (all (\line -> length line == width) lines)
      height = length lines
      moves = map (\(x, y, char) -> ((x, y), if isLower char then Red else Black)) $
                sortOn (\(_, _, char) -> (toLower char, isLower char))
                [ (x, y, char)
                | (y, line) <- zip [0 ..] lines
                , (x, char) <- zip [0 ..] line
                , toLower char /= toUpper char
                ]
  in assertion $ foldl (\field (pos, player) -> putPoint pos player field) (emptyField width height) moves

simpleSurround :: Assertion
simpleSurround =
  let image = " .a. \
              \ cBa \
              \ .a. "
      field = constructField image
  in do scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 4

main :: IO ()
main = defaultMain
  [ testCase "simple surround" simpleSurround ]

