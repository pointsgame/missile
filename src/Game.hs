module Game where

import Data.Maybe
import Data.List
import Data.Tree
import Player
import Field
import Settings
import Auxiliary

data Game = Game { curPlayer :: Player,
                   gameFields :: [Field],
                   gameTree :: Tree Field,
                   gameSettings :: Settings }

mapGameTreeMoves :: Tree Field -> (Pos -> Pos) -> Tree Field
mapGameTreeMoves (Node field children) f = Node field (map (\child -> mapGameTreeMoves' child field) children) where
  mapGameTreeMoves' (Node field' children') lastField =
    let (pos, player) = head $ moves field'
        newField = putPoint (f pos) player lastField
    in Node newField $ map (\child -> mapGameTreeMoves' child newField) children'

selectFields :: Tree Field -> [(Pos, Player)] -> [Field]
selectFields (Node field _) [] = [field]
selectFields (Node field children) (h : t) =
  field : selectFields (fromJust $ find (\(Node childField _) -> head (moves childField) == h) children) t

emptyGame :: Settings -> Game
emptyGame settings =
  let field = emptyField (gameWidth settings) (gameHeight settings)
  in Game { curPlayer = Red,
            gameFields = [field],
            gameTree = Node field [],
            gameSettings = settings }

updateGameTree :: [Field] -> Tree Field -> Tree Field
updateGameTree [] _ = error "updateGameTree: bug."
updateGameTree [newField] (Node field children) =
  Node field $ if isNothing $ find (\(Node childField _) -> head (moves childField) == head (moves newField)) children
               then Node newField [] : children
               else children
updateGameTree (h : t) (Node field children) =
  let newChildren = replaceSingle (\(Node childField _) -> head (moves childField) == head (moves h)) (updateGameTree t) children
  in Node field newChildren

putGamePlayersPoint :: Pos -> Player -> Game -> Game
putGamePlayersPoint pos player game =
  let fields = gameFields game
      newFields = putPoint pos player (head fields) : fields
  in game { curPlayer = nextPlayer player,
            gameFields = newFields,
            gameTree = updateGameTree (tail $ reverse newFields) (gameTree game) }

putGamePoint :: Pos -> Game -> Game
putGamePoint pos game = putGamePlayersPoint pos (curPlayer game) game

backGame :: Game -> Game
backGame game =
  game { curPlayer = snd $ head $ moves $ head $ gameFields game,
         gameFields = tail $ gameFields game }

reflectHorizontallyGame :: Game -> Game
reflectHorizontallyGame game =
  let width = fieldWidth $ rootLabel $ gameTree game
      f (posX, posY) = (width - posX - 1, posY)
      newTree = mapGameTreeMoves (gameTree game) f
  in game { gameFields = reverse $ selectFields newTree $ map (\(pos, player) -> (f pos, player)) $ reverse $ moves $ head $ gameFields game,
            gameTree = newTree }

reflectVerticallyGame :: Game -> Game
reflectVerticallyGame game =
  let height = fieldHeight $ rootLabel $ gameTree game
      f (posX, posY) = (posX, height - posY - 1)
      newTree = mapGameTreeMoves (gameTree game) f
  in game { gameFields = reverse $ selectFields newTree $ map (\(pos, player) -> (f pos, player)) $ reverse $ moves $ head $ gameFields game,
            gameTree = newTree }

updateGameSettings' :: Settings -> Settings -> Settings
updateGameSettings' oldSettings newSettings =
  newSettings { gameWidth = gameWidth oldSettings,
                gameHeight = gameHeight oldSettings }

updateGameSettings :: Game -> Settings -> Game
updateGameSettings game settings =
  game { gameSettings = updateGameSettings' (gameSettings game) settings }

gameIsEmpty :: Game -> Bool
gameIsEmpty game = null $ subForest $ gameTree game
