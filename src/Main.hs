module Main (main) where

import Data.Monoid
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Options.Applicative
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import Field
import GameTree
import Game
import Rendering
import GameSettings
import Cli

data MainWindow = MainWindow { mwWindow :: Gtk.Window
                             , mwNewImageMenuItem :: Gtk.ImageMenuItem
                             , mwOpenImageMenuItem :: Gtk.ImageMenuItem
                             , mwSaveImageMenuItem :: Gtk.ImageMenuItem
                             , mwCloseImageMenuItem :: Gtk.ImageMenuItem
                             , mwQuitImageMenuItem :: Gtk.ImageMenuItem
                             , mwUndoImageMenuItem :: Gtk.ImageMenuItem
                             , mwPreferencesImageMenuItem :: Gtk.ImageMenuItem
                             , mwAboutImageMenuItem :: Gtk.ImageMenuItem
                             , mwDrawingArea :: Gtk.DrawingArea
                             , mwCoordLabel :: Gtk.Label
                             }

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
  closeImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockClose
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
  Gtk.containerAdd fileMenu closeImageMenuItem
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
                    , mwCloseImageMenuItem = closeImageMenuItem
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
      field <- fmap head $ gameFields game
      updateCoordLabel (mwCoordLabel mainWindow) (mwDrawingArea mainWindow) field drawSettings x y
    return False
  _ <- mwCoordLabel mainWindow `Gtk.on` Gtk.draw $ liftIO $ do
    (x, y) <- Gtk.widgetGetPointer $ mwDrawingArea mainWindow
    drawSettings <- readIORef drawSettingsIORef
    game <- readIORef gameIORef
    field <- fmap head $ gameFields game
    updateCoordLabel (mwCoordLabel mainWindow) (mwDrawingArea mainWindow) field drawSettings (fromIntegral x) (fromIntegral y)
  _ <- mwDrawingArea mainWindow `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do
    Gtk.LeftButton <- Gtk.eventButton
    (x, y) <- Gtk.eventCoordinates
    liftIO $ do
      drawSettings <- readIORef drawSettingsIORef
      game <- readIORef gameIORef
      field <- fmap head $ gameFields game
      withPos (mwDrawingArea mainWindow) field drawSettings x y $ \pos -> gamePutPoint game pos
  return ()

main :: IO ()
main = do
  cliArguments <- execParser $ info cliArgumentsParser (fullDesc <> progDesc "Points game.")
  _ <- Gtk.initGUI
  logo <- Gtk.pixbufNewFromFile "Logo.png"
  license <- readFile "LICENSE.txt"
  mainWindow <- mainWindowNew logo
  let gameTree = emptyGameTree (gsWidth $ cliGameSettings cliArguments) (gsHeight $ cliGameSettings cliArguments)
      callbackError = const $ return ()
      callback = Gtk.postGUIAsync $ Gtk.widgetQueueDraw $ mwDrawingArea mainWindow
  game <- gameNew gameTree callbackError callback
  gameInitBots game (gsRedBotPath $ cliGameSettings cliArguments) (gsBlackBotPath $ cliGameSettings cliArguments)
  gameIORef <- newIORef game
  drawSettingsIORef <- newIORef $ cliDawSettings cliArguments
  listenMainWindow mainWindow logo license drawSettingsIORef gameIORef
  Gtk.widgetShowAll (mwWindow mainWindow)
  Gtk.mainGUI
