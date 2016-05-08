module VersusTable ( header1
                   , header2
                   , row
                   , footer
                   ) where

import Player
import Field

playerWidth :: Int
playerWidth = 6

posWidth :: Int
posWidth = 8

scoreWidth :: Int
scoreWidth = 7

borderWidth :: Int
borderWidth = length border

cornerWidth :: Int
cornerWidth = length leftCorner

tableWidth :: Int
tableWidth =
  cornerWidth +
  playerWidth +
  borderWidth +
  posWidth +
  borderWidth +
  scoreWidth +
  cornerWidth

append :: Char -> String -> Int -> String
append c str size =
  let len = length str
  in if | len > size -> take size str
        | len < size -> str ++ replicate (size - len) c
        | otherwise  -> str

topLeftCorner :: String
topLeftCorner = "/-"

topRightCorner :: String
topRightCorner = "-\\"

bottomLeftCorner :: String
bottomLeftCorner = "\\-"

bottomRightCorner :: String
bottomRightCorner = "-/"

border :: String
border = " | "

leftCorner :: String
leftCorner = "| "

rightCorner :: String
rightCorner = " |"

topCorner :: Char
topCorner = '-'

bottomCorner :: Char
bottomCorner = '-'

header1 :: Int -> String
header1 gameNumber =
  let game = "Game"
      title = game ++
              [topCorner] ++
              show gameNumber
  in topLeftCorner ++
     append topCorner title (tableWidth - 2 * cornerWidth) ++
     topRightCorner

header2 :: String
header2 =
  let player = "Player"
      pos = "Pos"
      score = "Score"
  in leftCorner ++
     append ' ' player playerWidth ++
     border ++
     append ' ' pos posWidth ++
     border ++
     append ' ' score scoreWidth ++
     rightCorner

row :: Player -> Pos -> Int -> Int -> String
row player pos scoreRed' scoreBlack' =
  leftCorner ++
  append ' ' (show player) playerWidth ++
  border ++
  append ' ' (show pos) posWidth ++
  border ++
  append ' ' (show scoreRed' ++ ":" ++ show scoreBlack') scoreWidth ++
  rightCorner

footer :: String
footer =
  bottomLeftCorner ++
  replicate (tableWidth - 2 * cornerWidth) bottomCorner ++
  bottomRightCorner
