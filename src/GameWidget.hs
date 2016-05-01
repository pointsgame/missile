module GameWidget ( GameWidget
                  , gameWidgetNew
                  , onClick
                  , toWidget
                  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Graphics.UI.Gtk as Gtk
import Field
import Rendering

data GameWidget = FullGameWidget { gwFieldsIO :: IO [Field]
                                 , gwDrawSettingsIO :: IO DrawSettings
                                 , gwTable :: Gtk.Table
                                 , gwDrawingArea :: Gtk.DrawingArea
                                 } |
                  LightGameWidget { gwFieldsIO :: IO [Field]
                                  , gwDrawSettingsIO :: IO DrawSettings
                                  , gwDrawingArea :: Gtk.DrawingArea
                                  }

withPos :: Gtk.DrawingArea -> IO [Field] -> IO DrawSettings -> Double -> Double -> (Pos -> IO ()) -> IO ()
withPos drawingArea fieldsIO drawSettingsIO x y f = do
  width <- fromIntegral <$> Gtk.widgetGetAllocatedWidth drawingArea
  height <- fromIntegral <$> Gtk.widgetGetAllocatedHeight drawingArea
  fields <- fieldsIO
  drawSettings <- drawSettingsIO
  let headField = head fields
      fieldWidth' = fieldWidth headField
      fieldHeight' = fieldHeight headField
      hReflection' = dsHReflection drawSettings
      vReflection' = dsVReflection drawSettings
      (_, _, toGamePosX, toGamePosY) = fromToFieldPos hReflection' vReflection' fieldWidth' fieldHeight' width height
      posX = toGamePosX x
      posY = toGamePosY y
  when (posX >= 0 && posY >= 0 && posX < fieldWidth' && posY < fieldHeight') $ f (posX, posY)

gameWidgetNew :: Bool -> IO [Field] -> IO DrawSettings -> IO GameWidget
gameWidgetNew light fieldsIO drawSettingsIO =
  do drawingArea <- Gtk.drawingAreaNew
     drawingArea `Gtk.on` Gtk.draw $ do
       fields <- liftIO fieldsIO
       drawSettings <- liftIO drawSettingsIO
       width' <- liftIO $ Gtk.widgetGetAllocatedWidth drawingArea
       height' <- liftIO $ Gtk.widgetGetAllocatedHeight drawingArea
       draw drawSettings (fromIntegral width') (fromIntegral height') fields
     if light
       then return LightGameWidget { gwFieldsIO = fieldsIO
                                   , gwDrawSettingsIO = drawSettingsIO
                                   , gwDrawingArea = drawingArea
                                   }
       else do
         table <- Gtk.tableNew 2 1 False
         coordLabel <- Gtk.labelNew (Nothing :: Maybe String)
         let updateCoordLabel x y = withPos drawingArea fieldsIO drawSettingsIO x y $ \(posX, posY) -> do
               let text = show (posX + 1) ++ ":" ++ show (posY + 1)
               labelText <- Gtk.labelGetText coordLabel
               when (labelText /= text) $ Gtk.labelSetText coordLabel text
         Gtk.tableAttachDefaults table drawingArea 0 1 0 1
         Gtk.tableAttach table coordLabel 0 1 1 2 [] [] 1 1
         drawingArea `Gtk.on` Gtk.motionNotifyEvent $ do
           (x, y) <- Gtk.eventCoordinates
           liftIO $ updateCoordLabel x y
           return False
         coordLabel `Gtk.on` Gtk.draw $ do
           (x, y) <- liftIO $ Gtk.widgetGetPointer drawingArea
           liftIO $ updateCoordLabel (fromIntegral x) (fromIntegral y)
         Gtk.widgetAddEvents drawingArea [Gtk.PointerMotionMask]
         return FullGameWidget { gwFieldsIO = fieldsIO
                               , gwDrawSettingsIO = drawSettingsIO
                               , gwTable = table
                               , gwDrawingArea = drawingArea
                               }

onClick :: GameWidget -> (Pos -> IO ()) -> IO (Gtk.ConnectId Gtk.DrawingArea)
onClick gameWidget click =
  gwDrawingArea gameWidget `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do
    Gtk.LeftButton <- Gtk.eventButton
    (x, y) <- Gtk.eventCoordinates
    liftIO $ withPos (gwDrawingArea gameWidget) (gwFieldsIO gameWidget) (gwDrawSettingsIO gameWidget) x y click

toWidget :: GameWidget -> Gtk.Widget
toWidget LightGameWidget { gwDrawingArea = drawingArea } = Gtk.toWidget drawingArea
toWidget FullGameWidget { gwTable = table } = Gtk.toWidget table
