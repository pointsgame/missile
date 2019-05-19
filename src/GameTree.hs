module GameTree ( GameTree(..)
                , isGameTreePuttingAllowed
                , selectGameTreeFields
                , emptyGameTree
                , buildGameTree
                , putGameTreePlayersPoint
                , putGameTreePoint
                , gameTreeBack
                , gameTreeIsEmpty
                , gameTreeIsOver
                , gameTreeWidth
                , gameTreeHeight
                ) where

import Data.List
import Data.Maybe
import Data.Tree
import Player
import Field

data GameTree = GameTree { gtCurPlayer :: Player
                         , gtFields :: [Field]
                         , gtTree :: Tree Field
                         }

isGameTreePuttingAllowed :: GameTree -> Pos -> Bool
isGameTreePuttingAllowed = isPuttingAllowed . head . gtFields

findByLastMove :: (Pos, Player) -> Forest Field -> Maybe (Tree Field)
findByLastMove move = find (\(Node field _) -> head (moves field) == move)

filterByLastMove :: (Pos, Player) -> Forest Field -> Forest Field
filterByLastMove move = filter (\(Node field _) -> head (moves field) /= move)

selectGameTreeFields :: Tree Field -> [(Pos, Player)] -> [Field]
selectGameTreeFields (Node field _) [] = [field]
selectGameTreeFields (Node field children) (h : t) =
  case findByLastMove h children of
    Just child -> field : selectGameTreeFields child t
    Nothing -> [field]

emptyGameTree :: Int -> Int -> GameTree
emptyGameTree width height =
  let field = emptyField width height
  in GameTree { gtCurPlayer = Red
              , gtFields = [field]
              , gtTree = Node field []
              }

unfoldFunction :: [Field] -> (Field, [[Field]])
unfoldFunction [] = error "unfoldFunction: can't unfold empty list of fields."
unfoldFunction [field] = (field, [])
unfoldFunction (field : t) = (field, [t])

buildGameTree :: [Field] -> GameTree
buildGameTree fields =
  let curPlayer = maybe Red (nextPlayer . snd) $ listToMaybe $ moves $ head fields
  in GameTree { gtCurPlayer = curPlayer
              , gtFields = fields
              , gtTree = unfoldTree unfoldFunction (reverse fields)
              }

updateGameTree :: [Field] -> Tree Field -> Tree Field
updateGameTree [] tree = tree
updateGameTree (h : t) (Node field children) =
  let move = head (moves h)
  in case findByLastMove move children of
       Just child -> Node field $ updateGameTree t child : filterByLastMove move children
       Nothing -> Node field $ updateGameTree t (Node h []) : children

putGameTreePlayersPoint :: Pos -> Player -> GameTree -> GameTree
putGameTreePlayersPoint pos player gameTree =
  let fields = gtFields gameTree
      newFields = putPoint pos player (head fields) : fields
  in gameTree { gtCurPlayer = nextPlayer player
              , gtFields = newFields
              , gtTree = updateGameTree (tail $ reverse newFields) (gtTree gameTree)
              }

putGameTreePoint :: Pos -> GameTree -> GameTree
putGameTreePoint pos gameTree = putGameTreePlayersPoint pos (gtCurPlayer gameTree) gameTree

gameTreeBack :: GameTree -> GameTree
gameTreeBack gameTree =
  gameTree { gtCurPlayer = snd $ head $ moves $ head $ gtFields gameTree
           , gtFields = tail $ gtFields gameTree
           }

gameTreeIsEmpty :: GameTree -> Bool
gameTreeIsEmpty = null . subForest . gtTree

gameTreeIsOver :: GameTree -> Bool
gameTreeIsOver = fieldIsFull . head . gtFields

gameTreeWidth :: GameTree -> Int
gameTreeWidth = fieldWidth . head . gtFields

gameTreeHeight :: GameTree -> Int
gameTreeHeight = fieldHeight . head . gtFields
