module Rendering where

import Data.Maybe
import Control.Monad
import Data.Colour.RGBSpace as Colour
import qualified Graphics.Rendering.Cairo as Cairo
import Player
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

setSourceRGBA :: RGB Double -> Double -> Cairo.Render ()
setSourceRGBA rgb = Cairo.setSourceRGBA (channelRed rgb) (channelGreen rgb) (channelBlue rgb)

setSourceRGB :: RGB Double -> Cairo.Render ()
setSourceRGB rgb = Cairo.setSourceRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)

polygon :: [(Double, Double)] -> Cairo.Render ()
polygon list =
  do uncurry Cairo.moveTo $ head list
     mapM_ (uncurry Cairo.lineTo) $ tail list
     Cairo.fill

draw :: Game -> Double -> Double -> Cairo.Render ()
draw game width height =
  do let fields = gameFields game
         headField = head fields
         gameFieldWidth = fromIntegral $ fieldWidth headField
         gameFieldHeight = fromIntegral $ fieldHeight headField
         width' = min width $ height / gameFieldHeight * gameFieldWidth
         height' = min height $ width / gameFieldWidth * gameFieldHeight
         shiftX = (width - width') / 2
         shiftY = (height - height') / 2
         settings = gameSettings game
         fromPosX = fromGamePosX game width height
         fromPosY = fromGamePosY game width height
         fromPos = fromGamePos game width height
         verticalLines = [fromPosX i | i <- [0..(fieldWidth headField - 1)]]
         horizontalLines = [fromPosY i | i <- [0..(fieldHeight headField - 1)]]
     --Rendering background.
     Cairo.setAntialias Cairo.AntialiasNone
     setSourceRGB $ backgroundColor $ gameSettings game
     Cairo.rectangle shiftX shiftY width' height'
     Cairo.fill
     --Rendering grig.
     Cairo.setLineWidth $ fromIntegral $ gridThickness settings
     setSourceRGB $ gridColor settings
     mapM_ (\x -> do Cairo.moveTo x shiftY
                     Cairo.lineTo x (shiftY + height')
                     Cairo.stroke) verticalLines
     mapM_ (\y -> do Cairo.moveTo shiftX y
                     Cairo.lineTo (shiftX + width') y
                     Cairo.stroke) horizontalLines
     --Rendering points.
     Cairo.setAntialias Cairo.AntialiasBest
     mapM_ (\((x, y), player) ->
       do setSourceRGB $ if player == Red then redColor settings else blackColor settings
          Cairo.arc (fromPosX x) (fromPosY y) (pointRadius settings * width' / gameFieldWidth / 5) 0 (2 * pi)
          Cairo.fill) $ moves headField
     --Rendering last point.
     unless (null $ moves headField) $ (\((x, y), player) ->
       do Cairo.setLineWidth 2
          setSourceRGB $ if player == Red then redColor settings else blackColor settings
          Cairo.arc (fromPosX x) (fromPosY y) (pointRadius settings * width' / gameFieldWidth / 3) 0 (2 * pi)
          Cairo.stroke) $ head $ moves headField
     --Rendering little surrounds.
     Cairo.setAntialias Cairo.AntialiasNone
     when (fullFill settings) $ mapM_ (\(field, (pos, player)) ->
       do if player == Red then setSourceRGBA (redColor settings) (fillingAlpha settings) else setSourceRGBA (blackColor settings) (fillingAlpha settings)
          if isPlayer field (s pos) player && isPlayer field (e pos) player
            then polygon [fromPos pos, fromPos $ s pos, fromPos $ e pos]
            else do when (isPlayer field (s pos) player && isPlayer field (se pos) player) $
                      polygon [fromPos pos, fromPos $ s pos, fromPos $ se pos]
                    when (isPlayer field (e pos) player && isPlayer field (se pos) player) $
                      polygon [fromPos pos, fromPos $ e pos, fromPos $ se pos]
          if isPlayer field (e pos) player && isPlayer field (n pos) player
            then polygon [fromPos pos, fromPos $ e pos, fromPos $ n pos]
            else do when (isPlayer field (e pos) player && isPlayer field (ne pos) player) $
                      polygon [fromPos pos, fromPos $ e pos, fromPos $ ne pos]
                    when (isPlayer field (n pos) player && isPlayer field (ne pos) player) $
                      polygon [fromPos pos, fromPos $ n pos, fromPos $ ne pos]
          if isPlayer field (n pos) player && isPlayer field (w pos) player
            then polygon [fromPos pos, fromPos $ n pos, fromPos $ w pos]
            else do when (isPlayer field (n pos) player && isPlayer field (nw pos) player) $
                      polygon [fromPos pos, fromPos $ n pos, fromPos $ nw pos]
                    when (isPlayer field (w pos) player && isPlayer field (nw pos) player) $
                      polygon [fromPos pos, fromPos $ w pos, fromPos $ nw pos]
          if isPlayer field (w pos) player && isPlayer field (s pos) player
            then polygon [fromPos pos, fromPos $ w pos, fromPos $ s pos]
            else do when (isPlayer field (w pos) player && isPlayer field (sw pos) player) $
                      polygon [fromPos pos, fromPos $ w pos, fromPos $ sw pos]
                    when (isPlayer field (s pos) player && isPlayer field (sw pos) player) $
                      polygon [fromPos pos, fromPos $ s pos, fromPos $ sw pos]) $ zip (reverse fields) (map (head . moves) $ tail $ reverse fields)
     --Rendering surrounds.
     mapM_ (\(chain, player) ->
       do if player == Red then setSourceRGBA (redColor settings) (fillingAlpha settings) else setSourceRGBA (blackColor settings) (fillingAlpha settings)
          polygon $ map fromPos chain) $ catMaybes $ map lastSurroundChain $ reverse fields
