module Rendering where

import Field
import Game
import Settings

fromPosXY :: Bool -> Double -> Int -> Int -> Double
fromPosXY reflection areaSize fieldSize x =
  let cellSize = areaSize / fromIntegral fieldSize
      x' = (fromIntegral x + 0.5) * cellSize
  in if reflection
     then areaSize - x'
     else x'

toPosXY :: Bool -> Double -> Int -> Double -> Int
toPosXY reflection areaSize fieldSize x =
  let cellSize = areaSize / fromIntegral fieldSize
      x' = floor $ x / cellSize
  in if reflection
     then fieldSize - x' - 1
     else x'

fromToGamePos :: Game -> Double -> Double -> (Int -> Double, Int -> Double, Double -> Int, Double -> Int)
fromToGamePos game width height =
  let fields = gameFields game
      headField = head fields
      gameFieldWidth = fromIntegral $ fieldWidth headField
      gameFieldHeight = fromIntegral $ fieldHeight headField
      width' = min width $ height / gameFieldHeight * gameFieldWidth
      height' = min height $ width / gameFieldWidth * gameFieldHeight
      shiftX = (width - width') / 2
      shiftY = (height - height') / 2
      settings = gameSettings game
  in ((shiftX +) . fromPosXY (horizontalReflection settings) width' (fieldWidth headField),
      (shiftY +) . fromPosXY (not $ verticalReflection settings) height' (fieldHeight headField),
      \coordX -> toPosXY (horizontalReflection settings) width' (fieldWidth headField) (coordX - shiftX),
      \coordY -> toPosXY (not $ verticalReflection settings) height' (fieldHeight headField) (coordY - shiftY))

fromGamePosX :: Game -> Double -> Double -> Int -> Double
fromGamePosX game width height =
  let (f, _, _, _) = fromToGamePos game width height
  in f

fromGamePosY :: Game -> Double -> Double -> Int -> Double
fromGamePosY game width height =
  let (_, f, _, _) = fromToGamePos game width height
  in f

fromGamePos :: Game -> Double -> Double -> Pos -> (Double, Double)
fromGamePos game width height (posX, posY) =
  let (f, g, _, _) = fromToGamePos game width height
  in (f posX, g posY)

toGamePosX :: Game -> Double -> Double -> Double -> Int
toGamePosX game width height =
  let (_, _, f, _) = fromToGamePos game width height
  in f

toGamePosY :: Game -> Double -> Double -> Double -> Int
toGamePosY game width height =
  let (_, _, _, f) = fromToGamePos game width height
  in f

toGamePos :: Game -> Double -> Double -> (Double, Double) -> Pos
toGamePos game width height (coordX, coordY) =
  let (_, _, f, g) = fromToGamePos game width height
  in (f coordX, g coordY)
