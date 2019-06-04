module Rendering ( fromToFieldPos
                 , draw
                 , DrawSettings(..)
                 ) where

import Data.Default
import Data.Maybe
import Control.Monad
import Data.Colour.RGBSpace as Colour
import qualified GI.Cairo.Render as Cairo
import Player
import Field

data DrawSettings = DrawSettings { dsHReflection :: Bool
                                 , dsVReflection :: Bool
                                 , dsGridThickness :: Int
                                 , dsGridColor :: RGB Double
                                 , dsBackgroundColor :: RGB Double
                                 , dsRedColor :: RGB Double
                                 , dsBlackColor :: RGB Double
                                 , dsPointRadius :: Double
                                 , dsFillingAlpha :: Double
                                 , dsFullFill :: Bool
                                 }

instance Default DrawSettings where
  def = DrawSettings { dsHReflection = False
                     , dsVReflection = False
                     , dsGridThickness = 1
                     , dsGridColor  = RGB 0.3 0.3 0.3
                     , dsBackgroundColor = RGB 1 1 1
                     , dsRedColor = RGB 1 0 0
                     , dsBlackColor = RGB 0 0 0
                     , dsPointRadius = 1
                     , dsFillingAlpha = 0.5
                     , dsFullFill = True
                     }

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

shift :: Double -> Double -> Double
shift size balancedSize = (size - balancedSize) / 2

dimensions :: Int -> Int -> Double -> Double -> (Double, Double, Double, Double)
dimensions fieldWidth' fieldHeight' width height =
  let fieldHeight'' = fromIntegral fieldHeight'
      fieldWidth'' = fromIntegral fieldWidth'
      width' = min width $ height / fieldHeight'' * fieldWidth''
      height' = min height $ width / fieldWidth'' * fieldHeight''
      shiftX = shift width width'
      shiftY = shift height height'
  in (width', height', shiftX, shiftY)

fromToFieldPos :: Bool -> Bool -> Int -> Int -> Double -> Double -> (Int -> Double, Int -> Double, Double -> Int, Double -> Int)
fromToFieldPos hReflection vReflection fieldWidth' fieldHeight' width height =
  let (width', height', shiftX, shiftY) = dimensions fieldWidth' fieldHeight' width height
  in ( (shiftX +) . fromPosXY hReflection width' fieldWidth' -- fromGamePosX
     , (shiftY +) . fromPosXY (not vReflection) height' fieldHeight' -- fromGamePosY
     , \coordX -> toPosXY hReflection width' fieldWidth' (coordX - shiftX) -- toGamePosX
     , \coordY -> toPosXY (not vReflection) height' fieldHeight' (coordY - shiftY) -- toGamePosY
     )

setSourceRGBA :: RGB Double -> Double -> Cairo.Render ()
setSourceRGBA rgb = Cairo.setSourceRGBA (channelRed rgb) (channelGreen rgb) (channelBlue rgb)

setSourceRGB :: RGB Double -> Cairo.Render ()
setSourceRGB rgb = Cairo.setSourceRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)

polygon :: [(Double, Double)] -> Cairo.Render ()
polygon list =
  do uncurry Cairo.moveTo $ head list
     mapM_ (uncurry Cairo.lineTo) $ tail list
     Cairo.fill

draw :: DrawSettings -> Double -> Double -> [Field] -> Cairo.Render ()
draw DrawSettings { dsHReflection = hReflection
                  , dsVReflection = vReflection
                  , dsGridThickness = gridThickness
                  , dsGridColor = gridColor
                  , dsBackgroundColor = backgroundColor
                  , dsRedColor = redColor
                  , dsBlackColor = blackColor
                  , dsPointRadius = pointRadius
                  , dsFillingAlpha = fillingAlpha
                  , dsFullFill = fullFill
                  } width height fields =
  do let headField = head fields
         fieldWidth' = fieldWidth headField
         fieldHeight' = fieldHeight headField
         (width', height', shiftX, shiftY) = dimensions fieldWidth' fieldHeight' width height
         scale = width' / fromIntegral fieldWidth'
         (fromPosX, fromPosY, _, _) = fromToFieldPos hReflection vReflection fieldWidth' fieldHeight' width height
         fromPos (x, y) = (fromPosX x, fromPosY y)
         verticalLines = [fromPosX i | i <- [0 .. (fieldWidth headField - 1)]]
         horizontalLines = [fromPosY i | i <- [0 .. (fieldHeight headField - 1)]]
     --Rendering background.
     Cairo.setAntialias Cairo.AntialiasNone
     setSourceRGB backgroundColor
     Cairo.rectangle shiftX shiftY width' height'
     Cairo.fill
     --Rendering grig.
     Cairo.setLineWidth $ fromIntegral gridThickness
     setSourceRGB gridColor
     mapM_ (\x -> do Cairo.moveTo x shiftY
                     Cairo.lineTo x (shiftY + height')
                     Cairo.stroke) verticalLines
     mapM_ (\y -> do Cairo.moveTo shiftX y
                     Cairo.lineTo (shiftX + width') y
                     Cairo.stroke) horizontalLines
     --Rendering points.
     Cairo.setAntialias Cairo.AntialiasBest
     mapM_ (\((x, y), player) ->
       do setSourceRGB $ if player == Red then redColor else blackColor
          Cairo.arc (fromPosX x) (fromPosY y) (pointRadius * scale / 5) 0 (2 * pi)
          Cairo.fill) $ moves headField
     --Rendering last point.
     unless (null $ moves headField) $ (\((x, y), player) ->
       do Cairo.setLineWidth 2
          setSourceRGB $ if player == Red then redColor else blackColor
          Cairo.arc (fromPosX x) (fromPosY y) (pointRadius * scale / 3) 0 (2 * pi)
          Cairo.stroke) $ head $ moves headField
     --Rendering little surrounds.
     Cairo.setAntialias Cairo.AntialiasNone
     when fullFill $ mapM_ (\(field, (pos, player)) ->
       do if player == Red then setSourceRGBA redColor fillingAlpha else setSourceRGBA blackColor fillingAlpha
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
       do if player == Red then setSourceRGBA redColor fillingAlpha else setSourceRGBA blackColor fillingAlpha
          polygon $ map fromPos chain) $ mapMaybe lastSurroundChain $ reverse fields
