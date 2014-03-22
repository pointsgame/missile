module Player where

data Player = Red | Black
  deriving (Eq, Show, Read)

nextPlayer :: Player -> Player
nextPlayer Red = Black
nextPlayer Black = Red
