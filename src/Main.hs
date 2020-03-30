{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedLists #-}

module Main (main) where

-- import Data.Colour.RGBSpace as Colour
-- import Data.IORef
-- import Data.Text ( Text
--                  , pack
--                  )
-- import Data.Text.IO as TextIO ( readFile
--                               )
-- import Data.Version ( showVersion
--                     )
-- import Control.Exception
-- import Control.Monad
-- import Options.Applicative
-- import Data.GI.Base ( get
--                     , new
--                     , on
--                     , set
--                     , AttrOp((:=))
--                     )
-- import Data.GI.Gtk.Threading ( postGUIASync
--                              )
-- import GI.Cairo.Render.Connector ( renderWithContext
--                                  )
-- import qualified GI.Cairo.Render as Cairo
-- import GI.Gdk.Flags ( EventMask ( EventMaskButtonPressMask
--                                 , EventMaskPointerMotionMask
--                                 )
--                     )
-- import GI.Gdk.Structs ( RGBA
--                       , newZeroRGBA
--                       )
-- import Paths_missile ( version
--                      )
-- import Async
-- import Field
-- import Game
-- import Rendering
-- import Cli

import Control.Monad (void)
import GI.GdkPixbuf.Objects.Pixbuf ( Pixbuf
                                   , pixbufNewFromFile
                                   )
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

-- data MainWindow = MainWindow { mwWindow :: Gtk.Window
--                              , mwNewImageMenuItem :: Gtk.ImageMenuItem
--                              , mwOpenImageMenuItem :: Gtk.ImageMenuItem
--                              , mwSaveImageMenuItem :: Gtk.ImageMenuItem
--                              , mwExportAsSvgMenuItem :: Gtk.MenuItem
--                              , mwExitImageMenuItem :: Gtk.ImageMenuItem
--                              , mwUndoImageMenuItem :: Gtk.ImageMenuItem
--                              , mwPreferencesImageMenuItem :: Gtk.ImageMenuItem
--                              , mwAboutImageMenuItem :: Gtk.ImageMenuItem
--                              , mwDrawingArea :: Gtk.DrawingArea
--                              , mwCoordLabel :: Gtk.Label
--                              }

-- data PreferencesDialog = PreferencesDialog { pdDialog :: Gtk.Dialog
--                                            , pdRedColorButton :: Gtk.ColorButton
--                                            , pdBlackColorButton :: Gtk.ColorButton
--                                            , pdBackgroundColorButton :: Gtk.ColorButton
--                                            , pdGridColorButton :: Gtk.ColorButton
--                                            , pdFillingAlphaSpinButton :: Gtk.SpinButton
--                                            , pdFullFillCheckButton :: Gtk.CheckButton
--                                            , pdGridThicknessSpinButton :: Gtk.SpinButton
--                                            , pdPointRadiusSpinButton :: Gtk.SpinButton
--                                            , pdHReflectionCheckButton :: Gtk.CheckButton
--                                            , pdVReflectionCheckButton :: Gtk.CheckButton
--                                            }

-- rgbToGtkRgba :: RGB Double -> IO RGBA
-- rgbToGtkRgba (RGB r g b) =
--   do rgba <- newZeroRGBA
--      rgba `set` [ #red := r
--                 , #green := g
--                 , #blue := b
--                 , #alpha := 1
--                 ]
--      return rgba

-- gtkRgbaToRgb :: RGBA -> IO (RGB Double)
-- gtkRgbaToRgb rgba =
--   do r <- rgba `get` #red
--      g <- rgba `get` #green
--      b <- rgba `get` #blue
--      return $ RGB r g b

-- getSettings :: DrawSettings -> PreferencesDialog -> IO DrawSettings
-- getSettings startSettings preferencesDialog =
--   do curRedColor <- pdRedColorButton preferencesDialog `get` #rgba >>= maybe (return $ dsRedColor startSettings) gtkRgbaToRgb
--      curBlackColor <- pdBlackColorButton preferencesDialog `get` #rgba >>= maybe (return $ dsBlackColor startSettings) gtkRgbaToRgb
--      curBackgroundColor <- pdBackgroundColorButton preferencesDialog `get` #rgba >>= maybe (return $ dsBackgroundColor startSettings) gtkRgbaToRgb
--      curGridColor <- pdGridColorButton preferencesDialog `get` #rgba >>= maybe (return $ dsGridColor startSettings) gtkRgbaToRgb
--      curFillingAlpha <- pdFillingAlphaSpinButton preferencesDialog `get` #value
--      curFullFill <- pdFullFillCheckButton preferencesDialog `get` #active
--      curGridThickness <- round <$> pdGridThicknessSpinButton preferencesDialog `get` #value
--      curPointRadius <- pdPointRadiusSpinButton preferencesDialog `get` #value
--      curHorizontalReflection <- pdHReflectionCheckButton preferencesDialog `get` #active
--      curVerticalReflection <- pdVReflectionCheckButton preferencesDialog `get` #active
--      return DrawSettings { dsHReflection = curHorizontalReflection
--                          , dsVReflection = curVerticalReflection
--                          , dsGridThickness = curGridThickness
--                          , dsGridColor = curGridColor
--                          , dsBackgroundColor = curBackgroundColor
--                          , dsRedColor = curRedColor
--                          , dsBlackColor = curBlackColor
--                          , dsPointRadius = curPointRadius
--                          , dsFillingAlpha = curFillingAlpha
--                          , dsFullFill = curFullFill
--                          }

-- preferencesDialogNew :: MainWindow -> DrawSettings -> IO PreferencesDialog
-- preferencesDialogNew mainWindow startSettings =
--   do
--      applyButton <- new Gtk.Button [ #label := "_Apply"
--                                    , #useUnderline := True
--                                    ]
--      okButton <- new Gtk.Button [ #label := "_Ok"
--                                 , #useUnderline := True
--                                 ]
--      cancelButton <- new Gtk.Button [ #label := "_Cancel"
--                                     , #useUnderline := True
--                                     ]
--      redColorLabel <- new Gtk.Label [ #label := "Red's color"
--                                     ]
--      redColor <- rgbToGtkRgba $ dsRedColor startSettings
--      redColorButton <- new Gtk.ColorButton [ #rgba := redColor
--                                            ]
--      blackColorLabel <- new Gtk.Label [ #label := "Black's color"
--                                       ]
--      blackColor <- rgbToGtkRgba $ dsBlackColor startSettings
--      blackColorButton <- new Gtk.ColorButton [ #rgba := blackColor
--                                              ]
--      backgroundColorLabel <- new Gtk.Label [ #label := "Background color"
--                                            ]
--      backgroundColor <- rgbToGtkRgba $ dsBackgroundColor startSettings
--      backgroundColorButton <- new Gtk.ColorButton [ #rgba := backgroundColor
--                                                   ]
--      gridColorLabel <- new Gtk.Label [ #label := "Grid color"
--                                      ]
--      gridColor <- rgbToGtkRgba $ dsGridColor startSettings
--      gridColorButton <- new Gtk.ColorButton [ #rgba := gridColor
--                                             ]
--      fillingAlphaLabel <- new Gtk.Label [ #label := "Filling alpha"
--                                         ]
--      fillingAlphaAdjustment <- new Gtk.Adjustment [ #lower := 0
--                                                   , #stepIncrement := 0.01
--                                                   , #upper := 1
--                                                   , #value := dsFillingAlpha startSettings
--                                                   ]
--      fillingAlphaSpinButton <- new Gtk.SpinButton [ #adjustment := fillingAlphaAdjustment
--                                                   , #digits := 2
--                                                   ]
--      fullFillCheckButton <- new Gtk.CheckButton [ #active := dsFullFill startSettings
--                                                 , #label := "Full fill"
--                                                 ]
--      gridThicknessLabel <- new Gtk.Label [ #label := "Grid thickness"
--                                          ]
--      gridThicknessAdjustment <- new Gtk.Adjustment [ #lower := 1
--                                                    , #stepIncrement := 1
--                                                    , #upper := 5
--                                                    , #value := fromIntegral $ dsGridThickness startSettings
--                                                    ]
--      gridThicknessSpinButton <- new Gtk.SpinButton [ #adjustment := gridThicknessAdjustment
--                                                    ]
--      pointRadiusLabel <- new Gtk.Label [ #label := "Point radius"
--                                        ]
--      pointRadiusAdjustment <- new Gtk.Adjustment [ #lower := 0.5
--                                                  , #stepIncrement := 0.1
--                                                  , #upper := 2
--                                                  , #value := dsPointRadius startSettings
--                                                  ]
--      pointRadiusSpinButton <- new Gtk.SpinButton [ #adjustment := pointRadiusAdjustment
--                                                  , #digits := 1
--                                                  ]
--      hReflectionCheckButton <- new Gtk.CheckButton [ #active := dsHReflection startSettings
--                                                    , #label := "Horizontal reflection"
--                                                    ]
--      vReflectionCheckButton <- new Gtk.CheckButton [ #active := dsVReflection startSettings
--                                                    , #label := "Vertical reflection"
--                                                    ]
--      grid <- new Gtk.Grid []
--      #attach grid redColorLabel 0 0 1 1
--      #attach grid redColorButton 1 0 1 1
--      #attach grid blackColorLabel 2 0 1 1
--      #attach grid blackColorButton 3 0 1 1
--      #attach grid backgroundColorLabel 0 1 1 1
--      #attach grid backgroundColorButton 1 1 1 1
--      #attach grid gridColorLabel 2 1 1 1
--      #attach grid gridColorButton 3 1 1 1
--      #attach grid fillingAlphaLabel 0 2 1 1
--      #attach grid fillingAlphaSpinButton 1 2 1 1
--      #attach grid fullFillCheckButton 2 2 2 1
--      #attach grid gridThicknessLabel 0 3 1 1
--      #attach grid gridThicknessSpinButton 1 3 1 1
--      #attach grid pointRadiusLabel 2 3 1 1
--      #attach grid pointRadiusSpinButton 3 3 1 1
--      #attach grid hReflectionCheckButton 0 4 2 1
--      #attach grid vReflectionCheckButton 2 4 2 1
--      frame <- new Gtk.Frame [ #label := "Draw settings"
--                             ]
--      #add frame grid
--      preferencesDialog <- new Gtk.Dialog [ #modal := True
--                                          , #title := "Draw settings"
--                                          , #transientFor := mwWindow mainWindow
--                                          ]
--      #addActionWidget preferencesDialog applyButton $ (fromIntegral . fromEnum) Gtk.ResponseTypeApply
--      #addActionWidget preferencesDialog okButton $ (fromIntegral . fromEnum) Gtk.ResponseTypeOk
--      #addActionWidget preferencesDialog cancelButton $ (fromIntegral . fromEnum) Gtk.ResponseTypeCancel
--      preferencesDialogContent <- #getContentArea preferencesDialog
--      #add preferencesDialogContent frame
--      return PreferencesDialog { pdDialog = preferencesDialog
--                               , pdRedColorButton = redColorButton
--                               , pdBlackColorButton = blackColorButton
--                               , pdBackgroundColorButton = backgroundColorButton
--                               , pdGridColorButton = gridColorButton
--                               , pdFillingAlphaSpinButton = fillingAlphaSpinButton
--                               , pdFullFillCheckButton = fullFillCheckButton
--                               , pdGridThicknessSpinButton = gridThicknessSpinButton
--                               , pdPointRadiusSpinButton = pointRadiusSpinButton
--                               , pdHReflectionCheckButton = hReflectionCheckButton
--                               , pdVReflectionCheckButton = vReflectionCheckButton
--                               }

-- runPreferencesDialog :: DrawSettings -> PreferencesDialog -> (DrawSettings -> IO ()) -> IO ()
-- runPreferencesDialog startSettings preferencesDialog f =
--   do #showAll $ pdDialog preferencesDialog
--      response <- fmap (toEnum . fromIntegral) $ #run $ pdDialog preferencesDialog
--      let apply = getSettings startSettings preferencesDialog >>= f
--          destroy = #destroy $ pdDialog preferencesDialog
--          continue = runPreferencesDialog startSettings preferencesDialog f
--      case response of
--        Gtk.ResponseTypeCancel      -> destroy
--        Gtk.ResponseTypeDeleteEvent -> destroy
--        Gtk.ResponseTypeApply       -> apply >> continue
--        Gtk.ResponseTypeOk          -> apply >> destroy
--        _                           -> error $ "runPreferencesDialog: unexpected response: " ++ show response

-- mainWindowNew :: Pixbuf -> IO MainWindow
-- mainWindowNew logo = do
--   newImage <- new Gtk.Image [ #iconName := "document-new"
--                             ]
--   newImageMenuItem <- new Gtk.ImageMenuItem [ #label := "_New"
--                                             , #useUnderline := True
--                                             , #image := newImage
--                                             ]
--   openImage <- new Gtk.Image [ #iconName := "document-open"
--                              ]
--   openImageMenuItem <- new Gtk.ImageMenuItem [ #label := "_Open"
--                                              , #useUnderline := True
--                                              , #image := openImage
--                                              ]
--   saveImage <- new Gtk.Image [ #iconName := "document-save"
--                              ]
--   saveImageMenuItem <- new Gtk.ImageMenuItem [ #label := "_Save"
--                                              , #useUnderline := True
--                                              , #image := saveImage
--                                              ]
--   exportAsSvgMenuItem <- new Gtk.MenuItem [ #label := "_Export as SVG"
--                                           , #useUnderline := True
--                                           ]
--   fileSeparatorMenuItem <- new Gtk.SeparatorMenuItem []
--   exitImage <- new Gtk.Image [ #iconName := "application-exit"
--                              ]
--   exitImageMenuItem <- new Gtk.ImageMenuItem [ #label := "_Exit"
--                                              , #useUnderline := True
--                                              , #image := exitImage
--                                              ]
--   fileMenu <- new Gtk.Menu []
--   #add fileMenu newImageMenuItem
--   #add fileMenu openImageMenuItem
--   #add fileMenu saveImageMenuItem
--   #add fileMenu exportAsSvgMenuItem
--   #add fileMenu fileSeparatorMenuItem
--   #add fileMenu exitImageMenuItem
--   fileMenuItem <- new Gtk.MenuItem [ #label := "_File"
--                                    , #submenu := fileMenu
--                                    , #useUnderline := True
--                                    ]
--   undoImage <- new Gtk.Image [ #iconName := "edit-undo"
--                              ]
--   undoImageMenuItem <- new Gtk.ImageMenuItem [ #label := "_Undo"
--                                              , #useUnderline := True
--                                              , #image := undoImage
--                                              ]
--   editSeparatorMenuItem <- new Gtk.SeparatorMenuItem []
--   preferencesImage <- new Gtk.Image [ #iconName := "document-properties"
--                                     ]
--   preferencesImageMenuItem <- new Gtk.ImageMenuItem [ #label := "_Preferences"
--                                                     , #useUnderline := True
--                                                     , #image := preferencesImage
--                                                     ]
--   editMenu <- new Gtk.Menu []
--   #add editMenu undoImageMenuItem
--   #add editMenu editSeparatorMenuItem
--   #add editMenu preferencesImageMenuItem
--   editMenuItem <- new Gtk.MenuItem [ #label := "_Edit"
--                                    , #submenu := editMenu
--                                    , #useUnderline := True
--                                    ]
--   aboutImage <- new Gtk.Image [ #iconName := "help-about"
--                               ]
--   aboutImageMenuItem <- new Gtk.ImageMenuItem [ #label := "_About"
--                                               , #useUnderline := True
--                                               , #image := aboutImage
--                                               ]
--   helpMenu <- new Gtk.Menu []
--   #add helpMenu aboutImageMenuItem
--   helpMenuItem <- new Gtk.MenuItem [ #label := "_Help"
--                                    , #submenu := helpMenu
--                                    , #useUnderline := True
--                                    ]
--   menuBar <- new Gtk.MenuBar []
--   #add menuBar fileMenuItem
--   #add menuBar editMenuItem
--   #add menuBar helpMenuItem
--   drawingArea <- new Gtk.DrawingArea []
--   #setHexpand drawingArea True
--   #setVexpand drawingArea True
--   #addEvents drawingArea [ EventMaskButtonPressMask
--                          , EventMaskPointerMotionMask
--                          ]
--   coordLabel <- new Gtk.Label []
--   grid <- new Gtk.Grid []
--   #attach grid drawingArea 0 0 1 1
--   #attach grid coordLabel 0 1 1 1
--   box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical
--                      ]
--   #add box menuBar
--   #add box grid
--   #setChildPacking box grid True True 0 Gtk.PackTypeStart
--   mainWindow <- new Gtk.Window [ #child := box
--                                , #defaultWidth := 800
--                                , #defaultHeight := 600
--                                , #icon := logo
--                                , #title := "Missile"
--                                ]
--   return MainWindow { mwWindow = mainWindow
--                     , mwNewImageMenuItem = newImageMenuItem
--                     , mwOpenImageMenuItem = openImageMenuItem
--                     , mwSaveImageMenuItem = saveImageMenuItem
--                     , mwExportAsSvgMenuItem = exportAsSvgMenuItem
--                     , mwExitImageMenuItem = exitImageMenuItem
--                     , mwUndoImageMenuItem = undoImageMenuItem
--                     , mwPreferencesImageMenuItem = preferencesImageMenuItem
--                     , mwAboutImageMenuItem = aboutImageMenuItem
--                     , mwDrawingArea = drawingArea
--                     , mwCoordLabel = coordLabel
--                     }

-- withPos :: Gtk.DrawingArea -> Field -> DrawSettings -> Double -> Double -> (Pos -> IO ()) -> IO ()
-- withPos drawingArea field drawSettings x y f = do
--   width <- fromIntegral <$> #getAllocatedWidth drawingArea
--   height <- fromIntegral <$> #getAllocatedHeight drawingArea
--   let fieldWidth' = fieldWidth field
--       fieldHeight' = fieldHeight field
--       hReflection' = dsHReflection drawSettings
--       vReflection' = dsVReflection drawSettings
--       (_, _, toGamePosX, toGamePosY) = fromToFieldPos hReflection' vReflection' fieldWidth' fieldHeight' width height
--       posX = toGamePosX x
--       posY = toGamePosY y
--   when (posX >= 0 && posY >= 0 && posX < fieldWidth' && posY < fieldHeight') $ f (posX, posY)

-- updateCoordLabel :: Gtk.Label -> Gtk.DrawingArea -> Field -> DrawSettings -> Double -> Double -> IO ()
-- updateCoordLabel coordLabel drawingArea field drawSettings x y =
--   withPos drawingArea field drawSettings x y $ \(posX, posY) -> do
--     let text = pack $ show (posX + 1) ++ ":" ++ show (posY + 1)
--     labelText <- coordLabel `get` #label
--     when (labelText /= text) $ coordLabel `set` [ #label := text
--                                                 ]

-- drawToSvg :: String -> DrawSettings -> [Field] -> IO ()
-- drawToSvg filename drawSettings fields =
--   let field = head fields
--       fieldWidth' = fieldWidth field
--       fieldHeight' = fieldHeight field
--       width = 800
--       height = width / fromIntegral fieldWidth' * fromIntegral fieldHeight'
--   in Cairo.withSVGSurface filename width height $ flip Cairo.renderWith $ draw drawSettings width height fields

-- listenMainWindow :: MainWindow -> Pixbuf -> Text -> IORef DrawSettings -> IORef Game -> (SomeException -> IO ()) -> IO ()
-- listenMainWindow mainWindow logo license drawSettingsIORef gameIORef callbackError = do
--   _ <- mwWindow mainWindow `on` #deleteEvent $ \_ -> do
--     game <- readIORef gameIORef
--     evalAsync (const $ return ()) $ gameStopBots game 1000000 >> now Gtk.mainQuit
--     return False
--   _ <- mwExitImageMenuItem mainWindow `on` #activate $ do
--     game <- readIORef gameIORef
--     #destroy $ mwWindow mainWindow
--     evalAsync (const $ return ()) $ gameStopBots game 1000000 >> now Gtk.mainQuit
--   _ <- mwAboutImageMenuItem mainWindow `on` #activate $ do
--     aboutDialog <- new Gtk.AboutDialog [ #authors := ["Evgeny Kurnevsky"]
--                                        , #license := license
--                                        , #logo := logo
--                                        , #programName := "Missile"
--                                        , #transientFor := mwWindow mainWindow
--                                        , #version := pack $ showVersion version
--                                        , #website := "https://gitlab.com/points/missile"
--                                        ]
--     _ <- #run aboutDialog
--     #destroy aboutDialog
--   _ <- mwPreferencesImageMenuItem mainWindow `on` #activate $ do
--     drawSettings <- readIORef drawSettingsIORef
--     preferencesDialog <- preferencesDialogNew mainWindow drawSettings
--     runPreferencesDialog drawSettings preferencesDialog $ \newSettings -> do
--       writeIORef drawSettingsIORef newSettings
--       #queueDraw $ mwDrawingArea mainWindow
--   _ <- mwExportAsSvgMenuItem mainWindow `on` #activate $ do
--     fileFilter <- new Gtk.FileFilter []
--     #addPattern fileFilter "*.svg"
--     fileChooserDialog <- new Gtk.FileChooserDialog [ #action := Gtk.FileChooserActionSave
--                                                    , #doOverwriteConfirmation := True
--                                                    , #filter := fileFilter
--                                                    , #transientFor := mwWindow mainWindow
--                                                    ]
--     _ <- #addButton fileChooserDialog "_Cancel" $ (fromIntegral . fromEnum) Gtk.ResponseTypeCancel
--     _ <- #addButton fileChooserDialog "_Save" $ (fromIntegral . fromEnum) Gtk.ResponseTypeAccept
--     response <- fmap (toEnum . fromIntegral) $ #run fileChooserDialog
--     case response of
--       Gtk.ResponseTypeCancel      -> #destroy fileChooserDialog
--       Gtk.ResponseTypeDeleteEvent -> #destroy fileChooserDialog
--       Gtk.ResponseTypeAccept      -> do maybeFilename <- #getFilename fileChooserDialog
--                                         case maybeFilename of
--                                           Just filename -> do
--                                             drawSettings <- readIORef drawSettingsIORef
--                                             game <- readIORef gameIORef
--                                             fields <- gameFields game
--                                             drawToSvg filename drawSettings fields
--                                           Nothing -> return ()
--                                         #destroy fileChooserDialog
--       _                           -> error $ "runFileChooserDialog: unexpected response: " ++ show response
--   _ <- mwDrawingArea mainWindow `on` #draw $ \context -> do
--     drawSettings <- readIORef drawSettingsIORef
--     game <- readIORef gameIORef
--     fields <- gameFields game
--     width' <- #getAllocatedWidth $ mwDrawingArea mainWindow
--     height' <- #getAllocatedHeight $ mwDrawingArea mainWindow
--     flip renderWithContext context $ draw drawSettings (fromIntegral width') (fromIntegral height') fields
--     return False
--   _ <- mwDrawingArea mainWindow `on` #motionNotifyEvent $ \event -> do
--     x <- event `get` #x
--     y <- event `get` #y
--     drawSettings <- readIORef drawSettingsIORef
--     game <- readIORef gameIORef
--     field <- head <$> gameFields game
--     updateCoordLabel (mwCoordLabel mainWindow) (mwDrawingArea mainWindow) field drawSettings x y
--     return False
--   _ <- mwDrawingArea mainWindow `on` #buttonPressEvent $ \event -> do
--     button <- event `get` #button
--     when (button == 1) $
--       do x <- event `get` #x
--          y <- event `get` #y
--          drawSettings <- readIORef drawSettingsIORef
--          game <- readIORef gameIORef
--          field <- head <$> gameFields game
--          withPos (mwDrawingArea mainWindow) field drawSettings x y $ evalAsync callbackError . gamePutPoint game
--     return False
--   return ()

-- main2 :: IO ()
-- main2 = do
--   cliArguments <- execParser $ info cliArgumentsParser (fullDesc <> progDesc "Points game.")
--   _ <- Gtk.init Nothing
--   logo <- pixbufNewFromFile "Logo.svg"
--   license <- TextIO.readFile "LICENSE.txt"
--   mainWindow <- mainWindowNew logo
--   let callbackError exception = postGUIASync $ do -- todo kill bots?
--         messageDialog <- new Gtk.MessageDialog [ #buttons := Gtk.ButtonsTypeOk
--                                                , #messageType := Gtk.MessageTypeError
--                                                , #text := pack $ show exception
--                                                , #transientFor := mwWindow mainWindow
--                                                ]
--         _ <- #run messageDialog
--         #destroy messageDialog
--       callback = postGUIASync $ #queueDraw $ mwDrawingArea mainWindow
--   game <- gameNew (cliGameSettings cliArguments) callback
--   evalAsync callbackError $ gameInitBots game
--   gameIORef <- newIORef game
--   let drawSettings = cliDrawSettings cliArguments
--   drawSettingsIORef <- newIORef drawSettings
--   listenMainWindow mainWindow logo license drawSettingsIORef gameIORef callbackError
--   #showAll (mwWindow mainWindow)
--   postGUIASync $ do (x, y) <- #getPointer $ mwDrawingArea mainWindow
--                     field <- head <$> gameFields game
--                     updateCoordLabel (mwCoordLabel mainWindow) (mwDrawingArea mainWindow) field drawSettings (fromIntegral x) (fromIntegral y)
--   Gtk.main








type State = ()

data Event = Closed

view' :: Pixbuf -> State -> AppView Gtk.Window Event
view' logo _ = bin
  Gtk.Window
  [ #defaultWidth := 800
  , #defaultHeight := 600
  , #icon := logo
  , #title := "Missile"
  , on #deleteEvent (const (True, Closed))
  ]
  $ container Gtk.Box
  [ #orientation := Gtk.OrientationVertical
  ]
  [ BoxChild
    { properties = defaultBoxChildProperties
    , child = container Gtk.MenuBar
              [
              ]
              [ subMenu
                "_File"
                [ widget Gtk.ImageMenuItem
                  [ #label := "_Exit"
                  , #useUnderline := True
                  -- , #image := exitImage
                  , on #activate Closed
                  ]
                  $ widget Gtk.Label
                  [ #label := "Open"
                  ]
                ]
              ]
    }
  ]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit

main :: IO ()
main = do
  logo <- pixbufNewFromFile "Logo.svg"
  void $ run App { view = view' logo
                 , update = update'
                 , inputs = []
                 , initialState = ()
                 }
