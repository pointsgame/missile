module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import FieldTests

main :: IO ()
main = defaultMain
  [ testCase "simple surround" simpleSurround
  , testCase "surround empty territory" surroundEmptyTerritory
  , testCase "move priority" movePriority
  , testCase "move priority, big" movePriorityBig
  , testCase "onion surroundings" onionSurroundings
  , testCase "apply 'control' surrounding in same turn" applyControlSurroundingInSameTurn
  , testCase "double surround" doubleSurround
  , testCase "double surround with empty part" doubleSurroundWithEmptyPart
  , testCase "should not leave empty inside" shouldNotLeaveEmptyInside
  , testCase "a hole inside a surrounding" holeInsideSurrounding
  , testCase "a hole inside a surrounding, after 'control' surrounding" holeInsideSurroundingAfterControlSurrounding
  , testCase "surrounding does not expand" surroundingDoesNotExpand
  , testCase "2 surroundings with common border" twoSurroundingsWithCommonBorder
  , testCase "3 surroundings with common borders" threeSurroundingsWithCommonBorders
  ]

