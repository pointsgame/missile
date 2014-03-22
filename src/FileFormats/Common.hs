module FileFormats.Common where

import Data.Tree
import Player
import Field
import Game

getMovesList :: Game -> [(Pos, Player)]
getMovesList game = getMovesList' $ gameTree game
        where getMovesList' (Node p []) = moves p
              getMovesList' (Node _ c) = getMovesList' $ head c