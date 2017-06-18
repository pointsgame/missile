module Main (main) where

import Data.Colour.RGBSpace as Colour
import Data.Colour.SRGB as SRGB
import Data.Monoid
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Options.Applicative
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import Field
import Game
import Rendering
import Cli

data MainWindow = MainWindow { mwWindow :: Gtk.Window
                             , mwNewImageMenuItem :: Gtk.ImageMenuItem
                             , mwOpenImageMenuItem :: Gtk.ImageMenuItem
                             , mwSaveImageMenuItem :: Gtk.ImageMenuItem
                             , mwQuitImageMenuItem :: Gtk.ImageMenuItem
                             , mwUndoImageMenuItem :: Gtk.ImageMenuItem
                             , mwPreferencesImageMenuItem :: Gtk.ImageMenuItem
                             , mwAboutImageMenuItem :: Gtk.ImageMenuItem
                             , mwDrawingArea :: Gtk.DrawingArea
                             , mwCoordLabel :: Gtk.Label
                             }

data PreferencesDialog = PreferencesDialog { pdDialog :: Gtk.Dialog
                                           , pdRedColorButton :: Gtk.ColorButton
                                           , pdBlackColorButton :: Gtk.ColorButton
                                           , pdBackgroundColorButton :: Gtk.ColorButton
                                           , pdGridColorButton :: Gtk.ColorButton
                                           , pdFillingAlphaSpinButton :: Gtk.SpinButton
                                           , pdFullFillCheckButton :: Gtk.CheckButton
                                           , pdGridThicknessSpinButton :: Gtk.SpinButton
                                           , pdPointRadiusSpinButton :: Gtk.SpinButton
                                           , pdHReflectionCheckButton :: Gtk.CheckButton
                                           , pdVReflectionCheckButton :: Gtk.CheckButton
                                           }

rgbToGtkColor :: RGB Double -> Gtk.Color
rgbToGtkColor (Colour.RGB r g b) =
  let Colour.RGB r' g' b' = toSRGBBounded $ sRGB r g b
  in Gtk.Color r' g' b'

gtkColorToRgb :: Gtk.Color -> RGB Double
gtkColorToRgb (Gtk.Color r g b) = toSRGB $ sRGBBounded r g b

getSettings :: PreferencesDialog -> IO DrawSettings
getSettings preferencesDialog =
  do curRedColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdRedColorButton preferencesDialog
     curBlackColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdBlackColorButton preferencesDialog
     curBackgroundColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdBackgroundColorButton preferencesDialog
     curGridColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdGridColorButton preferencesDialog
     curFillingAlpha <- Gtk.get (pdFillingAlphaSpinButton preferencesDialog) Gtk.spinButtonValue
     curFullFill <- Gtk.toggleButtonGetActive $ pdFullFillCheckButton preferencesDialog
     curGridThickness <- round <$> Gtk.get (pdGridThicknessSpinButton preferencesDialog) Gtk.spinButtonValue
     curPointRadius <- Gtk.get (pdPointRadiusSpinButton preferencesDialog) Gtk.spinButtonValue
     curHorizontalReflection <- Gtk.toggleButtonGetActive $ pdHReflectionCheckButton preferencesDialog
     curVerticalReflection <- Gtk.toggleButtonGetActive $ pdVReflectionCheckButton preferencesDialog
     return DrawSettings { dsHReflection = curHorizontalReflection
                         , dsVReflection = curVerticalReflection
                         , dsGridThickness = curGridThickness
                         , dsGridColor = curGridColor
                         , dsBackgroundColor = curBackgroundColor
                         , dsRedColor = curRedColor
                         , dsBlackColor = curBlackColor
                         , dsPointRadius = curPointRadius
                         , dsFillingAlpha = curFillingAlpha
                         , dsFullFill = curFullFill
                         }

preferencesDialogNew :: MainWindow -> DrawSettings -> IO PreferencesDialog
preferencesDialogNew mainWindow startSettings =
  do -- Create widgets.
     preferencesDialog <- Gtk.dialogNew
     preferencesDialogContent <- liftM Gtk.castToContainer $ Gtk.dialogGetContentArea preferencesDialog
     applyButton <- Gtk.buttonNewFromStock Gtk.stockApply
     okButton <- Gtk.buttonNewFromStock Gtk.stockOk
     cancelButton <- Gtk.buttonNewFromStock Gtk.stockCancel
     frame <- Gtk.frameNew
     table <- Gtk.tableNew 5 4 False
     redColorLabel <- Gtk.labelNew $ Just "Red's color"
     redColorButton <- Gtk.colorButtonNew
     blackColorLabel <- Gtk.labelNew $ Just "Black's color"
     blackColorButton <- Gtk.colorButtonNew
     backgroundColorLabel <- Gtk.labelNew $ Just "Background color"
     backgroundColorButton <- Gtk.colorButtonNew
     gridColorLabel <- Gtk.labelNew $ Just "Grid color"
     gridColorButton <- Gtk.colorButtonNew
     fillingAlphaLabel <- Gtk.labelNew $ Just "Filling alpha"
     fillingAlphaAdjustment <- Gtk.adjustmentNew (dsFillingAlpha startSettings) 0 1 0.01 0.01 0
     fillingAlphaSpinButton <- Gtk.spinButtonNew fillingAlphaAdjustment 0 2
     fullFillCheckButton <- Gtk.checkButtonNewWithLabel "Full fill"
     gridThicknessLabel <- Gtk.labelNew $ Just "Grid thickness"
     gridThicknessAdjustment <- Gtk.adjustmentNew (fromIntegral $ dsGridThickness startSettings) 1 5 1 1 0
     gridThicknessSpinButton <- Gtk.spinButtonNew gridThicknessAdjustment 0 0
     pointRadiusLabel <- Gtk.labelNew $ Just "Point radius"
     pointRadiusAdjustment <- Gtk.adjustmentNew (dsPointRadius startSettings) 0.1 10 0.1 0.1 0
     pointRadiusSpinButton <- Gtk.spinButtonNew pointRadiusAdjustment 0 1
     hReflectionCheckButton <- Gtk.checkButtonNewWithLabel "Horizontal reflection"
     vReflectionCheckButton <- Gtk.checkButtonNewWithLabel "Vertical reflection"
     -- Set properties.
     preferencesDialog `Gtk.set` [ Gtk.windowTransientFor := mwWindow mainWindow
                                 , Gtk.windowModal := True
                                 , Gtk.windowTitle := "Draw settings"
                                 ]
     Gtk.frameSetLabel frame "Draw settings"
     Gtk.colorButtonSetColor redColorButton $ rgbToGtkColor $ dsRedColor startSettings
     Gtk.colorButtonSetColor blackColorButton $ rgbToGtkColor $ dsBlackColor startSettings
     Gtk.colorButtonSetColor backgroundColorButton $ rgbToGtkColor $ dsBackgroundColor startSettings
     Gtk.colorButtonSetColor gridColorButton $ rgbToGtkColor $ dsGridColor startSettings
     Gtk.toggleButtonSetActive fullFillCheckButton $ dsFullFill startSettings
     Gtk.toggleButtonSetActive hReflectionCheckButton $ dsHReflection startSettings
     Gtk.toggleButtonSetActive vReflectionCheckButton $ dsVReflection startSettings
     -- Set hierarchy.
     Gtk.dialogAddActionWidget preferencesDialog applyButton Gtk.ResponseApply
     Gtk.dialogAddActionWidget preferencesDialog okButton Gtk.ResponseOk
     Gtk.dialogAddActionWidget preferencesDialog cancelButton Gtk.ResponseCancel
     Gtk.containerAdd preferencesDialogContent frame
     Gtk.containerAdd frame table
     Gtk.tableAttachDefaults table redColorLabel 0 1 0 1
     Gtk.tableAttachDefaults table redColorButton 1 2 0 1
     Gtk.tableAttachDefaults table blackColorLabel 2 3 0 1
     Gtk.tableAttachDefaults table blackColorButton 3 4 0 1
     Gtk.tableAttachDefaults table backgroundColorLabel 0 1 1 2
     Gtk.tableAttachDefaults table backgroundColorButton 1 2 1 2
     Gtk.tableAttachDefaults table gridColorLabel 2 3 1 2
     Gtk.tableAttachDefaults table gridColorButton 3 4 1 2
     Gtk.tableAttachDefaults table fillingAlphaLabel 0 1 2 3
     Gtk.tableAttachDefaults table fillingAlphaSpinButton 1 2 2 3
     Gtk.tableAttachDefaults table fullFillCheckButton 2 4 2 3
     Gtk.tableAttachDefaults table gridThicknessLabel 0 1 3 4
     Gtk.tableAttachDefaults table gridThicknessSpinButton 1 2 3 4
     Gtk.tableAttachDefaults table pointRadiusLabel 2 3 3 4
     Gtk.tableAttachDefaults table pointRadiusSpinButton 3 4 3 4
     Gtk.tableAttachDefaults table hReflectionCheckButton 0 2 4 5
     Gtk.tableAttachDefaults table vReflectionCheckButton 2 4 4 5
     -- Return dialog.
     return PreferencesDialog { pdDialog = preferencesDialog
                              , pdRedColorButton = redColorButton
                              , pdBlackColorButton = blackColorButton
                              , pdBackgroundColorButton = backgroundColorButton
                              , pdGridColorButton = gridColorButton
                              , pdFillingAlphaSpinButton = fillingAlphaSpinButton
                              , pdFullFillCheckButton = fullFillCheckButton
                              , pdGridThicknessSpinButton = gridThicknessSpinButton
                              , pdPointRadiusSpinButton = pointRadiusSpinButton
                              , pdHReflectionCheckButton = hReflectionCheckButton
                              , pdVReflectionCheckButton = vReflectionCheckButton
                              }

runPreferencesDialog :: DrawSettings -> PreferencesDialog -> (DrawSettings -> IO ()) -> IO ()
runPreferencesDialog startSettings preferencesDialog f =
  do Gtk.widgetShowAll $ pdDialog preferencesDialog
     response <- Gtk.dialogRun $ pdDialog preferencesDialog
     case response of
       Gtk.ResponseDeleteEvent -> Gtk.widgetDestroy $ pdDialog preferencesDialog
       Gtk.ResponseApply       -> do settings <- getSettings preferencesDialog
                                     f settings
                                     runPreferencesDialog startSettings preferencesDialog f
       Gtk.ResponseOk          -> do settings <- getSettings preferencesDialog
                                     f settings
                                     Gtk.widgetDestroy $ pdDialog preferencesDialog
       Gtk.ResponseCancel      -> Gtk.widgetDestroy $ pdDialog preferencesDialog
       _                       -> error $ "runPreferencesDialog: unexpected response: " ++ show response

mainWindowNew :: Gtk.Pixbuf -> IO MainWindow
mainWindowNew logo = do
  -- Create widgets.
  mainWindow <- Gtk.windowNew
  vbox <- Gtk.vBoxNew False 1
  menuBar <- Gtk.menuBarNew
  fileImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockFile
  fileMenu <- Gtk.menuNew
  newImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockNew
  openImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockOpen
  saveImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockSave
  fileSeparatorMenuItem <- Gtk.separatorMenuItemNew
  quitImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockQuit
  editImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockEdit
  editMenu <- Gtk.menuNew
  undoImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockUndo
  editSeparatorMenuItem <- Gtk.separatorMenuItemNew
  preferencesImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockPreferences
  helpImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockHelp
  helpMenu <- Gtk.menuNew
  aboutImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockAbout
  table <- Gtk.tableNew 2 1 False
  drawingArea <- Gtk.drawingAreaNew
  coordLabel <- Gtk.labelNew (Nothing :: Maybe String)
  -- Set properties.
  Gtk.windowSetDefaultSize mainWindow 800 600
  mainWindow `Gtk.set` [ Gtk.windowTitle := "Missile"
                       , Gtk.windowIcon := Just logo
                       , Gtk.containerChild := vbox
                       ]
  drawingArea `Gtk.widgetAddEvents` [Gtk.PointerMotionMask]
  -- Set hierarchy.
  Gtk.containerAdd vbox menuBar
  vbox `Gtk.set` [Gtk.boxChildPacking menuBar := Gtk.PackNatural]
  Gtk.containerAdd menuBar fileImageMenuItem
  fileImageMenuItem `Gtk.set` [Gtk.menuItemSubmenu := fileMenu]
  Gtk.containerAdd fileMenu newImageMenuItem
  Gtk.containerAdd fileMenu openImageMenuItem
  Gtk.containerAdd fileMenu saveImageMenuItem
  Gtk.containerAdd fileMenu fileSeparatorMenuItem
  Gtk.containerAdd fileMenu quitImageMenuItem
  Gtk.containerAdd menuBar editImageMenuItem
  editImageMenuItem `Gtk.set` [Gtk.menuItemSubmenu := editMenu]
  Gtk.containerAdd editMenu undoImageMenuItem
  Gtk.containerAdd editMenu editSeparatorMenuItem
  Gtk.containerAdd editMenu preferencesImageMenuItem
  Gtk.containerAdd menuBar helpImageMenuItem
  helpImageMenuItem `Gtk.set` [Gtk.menuItemSubmenu := helpMenu]
  Gtk.containerAdd helpMenu aboutImageMenuItem
  Gtk.containerAdd vbox table
  Gtk.tableAttachDefaults table drawingArea 0 1 0 1
  Gtk.tableAttach table coordLabel 0 1 1 2 [] [] 1 1
  -- Return window.
  return MainWindow { mwWindow = mainWindow
                    , mwNewImageMenuItem = newImageMenuItem
                    , mwOpenImageMenuItem = openImageMenuItem
                    , mwSaveImageMenuItem = saveImageMenuItem
                    , mwQuitImageMenuItem = quitImageMenuItem
                    , mwUndoImageMenuItem = undoImageMenuItem
                    , mwPreferencesImageMenuItem = preferencesImageMenuItem
                    , mwAboutImageMenuItem = aboutImageMenuItem
                    , mwDrawingArea = drawingArea
                    , mwCoordLabel = coordLabel
                    }

withPos :: Gtk.DrawingArea -> Field -> DrawSettings -> Double -> Double -> (Pos -> IO ()) -> IO ()
withPos drawingArea field drawSettings x y f = do
  width <- fromIntegral <$> Gtk.widgetGetAllocatedWidth drawingArea
  height <- fromIntegral <$> Gtk.widgetGetAllocatedHeight drawingArea
  let fieldWidth' = fieldWidth field
      fieldHeight' = fieldHeight field
      hReflection' = dsHReflection drawSettings
      vReflection' = dsVReflection drawSettings
      (_, _, toGamePosX, toGamePosY) = fromToFieldPos hReflection' vReflection' fieldWidth' fieldHeight' width height
      posX = toGamePosX x
      posY = toGamePosY y
  when (posX >= 0 && posY >= 0 && posX < fieldWidth' && posY < fieldHeight') $ f (posX, posY)

updateCoordLabel :: Gtk.Label -> Gtk.DrawingArea -> Field -> DrawSettings -> Double -> Double -> IO ()
updateCoordLabel coordLabel drawingArea field drawSettings x y =
  withPos drawingArea field drawSettings x y $ \(posX, posY) -> do
    let text = show (posX + 1) ++ ":" ++ show (posY + 1)
    labelText <- Gtk.labelGetText coordLabel
    when (labelText /= text) $ Gtk.labelSetText coordLabel text

listenMainWindow :: MainWindow -> Gtk.Pixbuf -> String -> IORef DrawSettings -> IORef Game -> IO ()
listenMainWindow mainWindow logo license drawSettingsIORef gameIORef = do
  _ <- mwWindow mainWindow `Gtk.on` Gtk.deleteEvent $ liftIO $ do
    game <- liftIO $ readIORef gameIORef
    gameStopBots game 1000000 Gtk.mainQuit
    return False
  _ <- mwQuitImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $ do
    game <- liftIO $ readIORef gameIORef
    Gtk.widgetDestroy $ mwWindow mainWindow
    gameStopBots game 1000000 Gtk.mainQuit
  _ <- mwAboutImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $ do
    aboutDialog <- Gtk.aboutDialogNew
    aboutDialog `Gtk.set` [ Gtk.windowTransientFor := mwWindow mainWindow
                          , Gtk.aboutDialogProgramName := "Missile"
                          , Gtk.aboutDialogVersion := "3.0.0"
                          , Gtk.aboutDialogLicense := Just license
                          , Gtk.aboutDialogWebsite := "https://gitlab.com/points/missile"
                          , Gtk.aboutDialogAuthors := ["Evgeny Kurnevsky"]
                          , Gtk.aboutDialogLogo := Just logo
                          ]
    _ <- Gtk.dialogRun aboutDialog
    Gtk.widgetDestroy aboutDialog
  _ <- mwPreferencesImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $ do
    drawSettings <- readIORef drawSettingsIORef
    preferencesDialog <- preferencesDialogNew mainWindow drawSettings
    runPreferencesDialog drawSettings preferencesDialog $ \newSettings -> do
      writeIORef drawSettingsIORef newSettings
      Gtk.widgetQueueDraw $ mwDrawingArea mainWindow
  _ <- mwDrawingArea mainWindow `Gtk.on` Gtk.draw $ do
    drawSettings <- liftIO $ readIORef drawSettingsIORef
    game <- liftIO $ readIORef gameIORef
    fields <- liftIO $ gameFields game
    width' <- liftIO $ Gtk.widgetGetAllocatedWidth $ mwDrawingArea mainWindow
    height' <- liftIO $ Gtk.widgetGetAllocatedHeight $ mwDrawingArea mainWindow
    draw drawSettings (fromIntegral width') (fromIntegral height') fields
  _ <- mwDrawingArea mainWindow `Gtk.on` Gtk.motionNotifyEvent $ do
    (x, y) <- Gtk.eventCoordinates
    liftIO $ do
      drawSettings <- readIORef drawSettingsIORef
      game <- readIORef gameIORef
      field <- head <$> gameFields game
      updateCoordLabel (mwCoordLabel mainWindow) (mwDrawingArea mainWindow) field drawSettings x y
    return False
  _ <- mwCoordLabel mainWindow `Gtk.on` Gtk.draw $ liftIO $ do
    (x, y) <- Gtk.widgetGetPointer $ mwDrawingArea mainWindow
    drawSettings <- readIORef drawSettingsIORef
    game <- readIORef gameIORef
    field <- head <$> gameFields game
    updateCoordLabel (mwCoordLabel mainWindow) (mwDrawingArea mainWindow) field drawSettings (fromIntegral x) (fromIntegral y)
  _ <- mwDrawingArea mainWindow `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do
    Gtk.LeftButton <- Gtk.eventButton
    (x, y) <- Gtk.eventCoordinates
    liftIO $ do
      drawSettings <- readIORef drawSettingsIORef
      game <- readIORef gameIORef
      field <- head <$> gameFields game
      withPos (mwDrawingArea mainWindow) field drawSettings x y $ \pos -> gamePutPoint game pos
  return ()

main :: IO ()
main = do
  cliArguments <- execParser $ info cliArgumentsParser (fullDesc <> progDesc "Points game.")
  _ <- Gtk.initGUI
  logo <- Gtk.pixbufNewFromFile "Logo.png"
  license <- readFile "LICENSE.txt"
  mainWindow <- mainWindowNew logo
  let callbackError player = Gtk.postGUIAsync $ do
        messageDialog <- Gtk.messageDialogNew (Just $ mwWindow mainWindow) [] Gtk.MessageError Gtk.ButtonsOk $ show player ++ " bot made a mistake. It was killed."
        _ <- Gtk.dialogRun messageDialog
        Gtk.widgetDestroy messageDialog
      callback = Gtk.postGUIAsync $ Gtk.widgetQueueDraw $ mwDrawingArea mainWindow
  game <- gameNew (cliGameSettings cliArguments) callbackError callback
  gameInitBots game
  gameIORef <- newIORef game
  drawSettingsIORef <- newIORef $ cliDawSettings cliArguments
  listenMainWindow mainWindow logo license drawSettingsIORef gameIORef
  Gtk.widgetShowAll (mwWindow mainWindow)
  Gtk.mainGUI
