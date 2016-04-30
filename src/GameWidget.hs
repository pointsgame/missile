module GameWidget ( gameWidgetNew
                  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Graphics.UI.Gtk as Gtk
import Field
import Rendering

gameWidgetNew :: Bool -> IO [Field] -> IO DrawSettings -> (Pos -> IO Bool) -> IO Gtk.Widget
gameWidgetNew light fieldsIO drawSettingsIO click =
  do drawingArea <- Gtk.drawingAreaNew
     drawingArea `Gtk.on` Gtk.draw $ do
       fields <- liftIO fieldsIO
       drawSettings <- liftIO drawSettingsIO
       width' <- liftIO $ Gtk.widgetGetAllocatedWidth drawingArea
       height' <- liftIO $ Gtk.widgetGetAllocatedHeight drawingArea
       draw drawSettings (fromIntegral width') (fromIntegral height') fields
     let withPos x y g f = do
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
           if posX >= 0 && posY >= 0 && posX < fieldWidth' && posY < fieldHeight'
             then f (posX, posY)
             else g
     widget <- if light
       then return $ Gtk.toWidget drawingArea
       else do
         table <- Gtk.tableNew 2 1 False
         coordLabel <- Gtk.labelNew (Nothing :: Maybe String)
         let updateCoordLabel x y = withPos x y (return ()) $ \(posX, posY) -> do
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
         return $ Gtk.toWidget table
     drawingArea `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do
       Gtk.LeftButton <- Gtk.eventButton
       (x, y) <- Gtk.eventCoordinates
       liftIO $ do
         redraw <- withPos x y (return False) click
         when redraw $ Gtk.postGUIAsync $ Gtk.widgetQueueDraw widget
     return widget
