module Main (main) where

import Data.Maybe
import Data.StateVar
import qualified Data.IntMap as IntMap
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Colour.RGBSpace as Colour
import Data.Colour.SRGB as SRGB
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.Rendering.Cairo as Cairo
import Codec.Binary.UTF8.String
import Player
import Field
import Settings
import Game
import GameWithBot
import qualified FileFormats.XT as XT
import Rendering
import Auxiliary

data MainWindow = MainWindow { mwWindow :: Gtk.Window,
                               mwNewImageMenuItem :: Gtk.ImageMenuItem,
                               mwOpenImageMenuItem :: Gtk.ImageMenuItem,
                               mwSaveImageMenuItem :: Gtk.ImageMenuItem,
                               mwCloseImageMenuItem :: Gtk.ImageMenuItem,
                               mwQuitImageMenuItem :: Gtk.ImageMenuItem,
                               mwUndoImageMenuItem :: Gtk.ImageMenuItem,
                               mwReflectHorizontallyMenuItem :: Gtk.MenuItem,
                               mwReflectVerticallyMenuItem :: Gtk.MenuItem,
                               mwPreferencesImageMenuItem :: Gtk.ImageMenuItem,
                               mwAboutImageMenuItem :: Gtk.ImageMenuItem,
                               mwNotebook :: Gtk.Notebook }

data PreferencesDialog = PreferencesDialog { pdDialog :: Gtk.Dialog,
                                             pdGameNameEntry :: Gtk.Entry,
                                             pdFieldWidthSpinButton :: Gtk.SpinButton,
                                             pdFieldHeightSpinButton :: Gtk.SpinButton,
                                             pdRedNameEntry :: Gtk.Entry,
                                             pdBlackNameEntry :: Gtk.Entry,
                                             pdRedColorButton :: Gtk.ColorButton,
                                             pdBlackColorButton :: Gtk.ColorButton,
                                             pdBackgroundColorButton :: Gtk.ColorButton,
                                             pdGridColorButton :: Gtk.ColorButton,
                                             pdFillingAlphaSpinButton :: Gtk.SpinButton,
                                             pdHorizontalReflectionCheckButton :: Gtk.CheckButton,
                                             pdVerticalReflectionCheckButton :: Gtk.CheckButton,
                                             pdAiPresentCheckButton :: Gtk.CheckButton,
                                             pdAiPathFileChooserButton :: Gtk.FileChooserButton,
                                             pdAiRespondentCheckButton :: Gtk.CheckButton,
                                             pdSimpleRadioButton :: Gtk.RadioButton,
                                             pdWithTimeRadioButton :: Gtk.RadioButton,
                                             pdWithTimeSpinButton :: Gtk.SpinButton,
                                             pdWithComplexityRadioButton :: Gtk.RadioButton,
                                             pdWithComplexitySpinButton :: Gtk.SpinButton }

data GameTab = GameTab { gtWidget :: Gtk.Widget,
                         gtDrawingArea :: Gtk.DrawingArea,
                         gtCoordLabel :: Gtk.Label }

rgbToGtkColor :: RGB Double -> Gtk.Color
rgbToGtkColor (Colour.RGB r g b) =
  let Colour.RGB r' g' b' = toSRGBBounded $ sRGB r g b
  in Gtk.Color r' g' b'

gtkColorToRgb :: Gtk.Color -> RGB Double
gtkColorToRgb (Gtk.Color r g b) = toSRGB $ sRGBBounded r g b

getSettings :: Settings -> PreferencesDialog -> IO Settings
getSettings startSettings preferencesDialog =
  do curGameWidth <- liftM round $ Gtk.get (pdFieldWidthSpinButton preferencesDialog) Gtk.spinButtonValue
     curGameHeight <- liftM round $ Gtk.get (pdFieldHeightSpinButton preferencesDialog) Gtk.spinButtonValue
     curGameName <- liftM trim $ Gtk.entryGetText $ pdGameNameEntry preferencesDialog
     curRedName <- liftM trim $ Gtk.entryGetText $ pdRedNameEntry preferencesDialog
     curBlackName <- liftM trim $ Gtk.entryGetText $ pdBlackNameEntry preferencesDialog
     curRedColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdRedColorButton preferencesDialog
     curBlackColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdBlackColorButton preferencesDialog
     curBackgroundColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdBackgroundColorButton preferencesDialog
     curFillingAlpha <- Gtk.get (pdFillingAlphaSpinButton preferencesDialog) Gtk.spinButtonValue
     curGridColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor $ pdGridColorButton preferencesDialog
     curHorizontalReflection <- Gtk.toggleButtonGetActive $ pdHorizontalReflectionCheckButton preferencesDialog
     curVerticalReflection <- Gtk.toggleButtonGetActive $ pdVerticalReflectionCheckButton preferencesDialog
     curAiPresent <- Gtk.toggleButtonGetActive $ pdAiPresentCheckButton preferencesDialog
     curAiPathMaybe <- liftM (fmap decodeString) $ Gtk.fileChooserGetFilename $ pdAiPathFileChooserButton preferencesDialog
     curAiRespondent <- Gtk.toggleButtonGetActive $ pdAiRespondentCheckButton preferencesDialog
     curSimple <- Gtk.toggleButtonGetActive $ pdSimpleRadioButton preferencesDialog
     curWithTime <- Gtk.toggleButtonGetActive $ pdWithTimeRadioButton preferencesDialog
     curTime <- liftM (round . (* 1000)) $ Gtk.get (pdWithTimeSpinButton preferencesDialog) Gtk.spinButtonValue
     curWithComplexity <- Gtk.toggleButtonGetActive $ pdWithComplexityRadioButton preferencesDialog
     curComplexity <- liftM round $ Gtk.get (pdWithComplexitySpinButton preferencesDialog) Gtk.spinButtonValue
     return startSettings { gameWidth = curGameWidth,
                            gameHeight = curGameHeight,
                            gameName = curGameName,
                            redName = curRedName,
                            blackName = curBlackName,
                            redColor = curRedColor,
                            blackColor = curBlackColor,
                            backgroundColor = curBackgroundColor,
                            gridColor = curGridColor,
                            fillingAlpha = curFillingAlpha,
                            horizontalReflection = curHorizontalReflection,
                            verticalReflection = curVerticalReflection,
                            aiPresent = curAiPresent && isJust curAiPathMaybe,
                            aiPath = fromMaybe "" curAiPathMaybe,
                            aiRespondent = curAiRespondent,
                            aiGenMoveType = if | curSimple         -> Simple
                                               | curWithTime       -> WithTime curTime
                                               | curWithComplexity -> WithComplexity curComplexity
                                               | otherwise         -> error "Type of move generation not specified!" }

preferencesDialogNew :: Settings -> IO PreferencesDialog
preferencesDialogNew startSettings =
  do -- Create widgets.
     preferencesDialog <- Gtk.dialogNew
     preferencesDialogContent <- liftM Gtk.castToContainer $ Gtk.dialogGetContentArea preferencesDialog
     applyButton <- Gtk.buttonNewFromStock Gtk.stockApply
     okButton <- Gtk.buttonNewFromStock Gtk.stockOk
     cancelButton <- Gtk.buttonNewFromStock Gtk.stockCancel
     table <- Gtk.tableNew 1 4 False
     gameFrame <- Gtk.frameNew
     gameFrameTable <- Gtk.tableNew 3 2 False
     gameNameLabel <- Gtk.labelNew $ Just "Name"
     gameNameEntry <- Gtk.entryNew
     fieldWidthLabel <- Gtk.labelNew $ Just "Width"
     fieldWidthAdjustment <- Gtk.adjustmentNew (fromIntegral $ gameWidth startSettings) 1 999 1 1 0
     fieldWidthSpinButton <- Gtk.spinButtonNew fieldWidthAdjustment 0 0
     fieldHeightLabel <- Gtk.labelNew $ Just "Height"
     fieldHeightAdjustment <- Gtk.adjustmentNew (fromIntegral $ gameHeight startSettings) 1 999 1 1 0
     fieldHeightSpinButton <- Gtk.spinButtonNew fieldHeightAdjustment 0 0
     playersFrame <- Gtk.frameNew
     playersFrameTable <- Gtk.tableNew 4 2 False
     redNameLabel <- Gtk.labelNew $ Just "Red's name"
     redNameEntry <- Gtk.entryNew
     blackNameLabel <- Gtk.labelNew $ Just "Black's name"
     blackNameEntry <- Gtk.entryNew
     redColorLabel <- Gtk.labelNew $ Just "Red's color"
     redColorButton <- Gtk.colorButtonNew
     blackColorLabel <- Gtk.labelNew $ Just "Black's color"
     blackColorButton <- Gtk.colorButtonNew
     otherFrame <- Gtk.frameNew
     otherFrameTable <- Gtk.tableNew 5 2 False
     backgroundColorLabel <- Gtk.labelNew $ Just "Background color"
     backgroundColorButton <- Gtk.colorButtonNew
     gridColorLabel <- Gtk.labelNew $ Just "Grid color"
     gridColorButton <- Gtk.colorButtonNew
     fillingAlphaLabel <- Gtk.labelNew $ Just "Filling alpha"
     fillingAlphaAdjustment <- Gtk.adjustmentNew (fillingAlpha startSettings) 0 1 0.01 0.01 0
     fillingAlphaSpinButton <- Gtk.spinButtonNew fillingAlphaAdjustment 0 2
     horizontalReflectionCheckButton <- Gtk.checkButtonNewWithLabel "Horizontal reflection"
     verticalReflectionCheckButton <- Gtk.checkButtonNewWithLabel "Vertical reflection"
     aiFrame <- Gtk.frameNew
     aiFrameTable <- Gtk.tableNew 4 1 False
     aiPresentCheckButton <- Gtk.checkButtonNewWithLabel "AI is present"
     aiPathFileChooserButton <- Gtk.fileChooserButtonNew "Choose AI" Gtk.FileChooserActionOpen
     aiRespondentCheckButton <- Gtk.checkButtonNewWithLabel "AI is respondent"
     genMoveTypeFrame <- Gtk.frameNew
     genMoveTypeFrameTable <- Gtk.tableNew 3 2 False
     simpleRadioButton <- Gtk.radioButtonNewWithLabel "Simple"
     withTimeRadioButton <- Gtk.radioButtonNewWithLabelFromWidget simpleRadioButton "With time"
     withComplexityRadioButton <- Gtk.radioButtonNewWithLabelFromWidget simpleRadioButton "With complexity"
     (timeAdjustment, complexityAdjustment) <- case aiGenMoveType startSettings of
       Simple                    -> do timeAdjustment' <- Gtk.adjustmentNew 30 0 1000000 0.1 0.1 0
                                       complexityAdjustment' <- Gtk.adjustmentNew 50 0 100 1 1 0
                                       Gtk.toggleButtonSetActive simpleRadioButton True
                                       return (timeAdjustment', complexityAdjustment')
       WithTime time             -> do timeAdjustment' <- Gtk.adjustmentNew (fromIntegral time / 1000) 0 1000000 0.1 0.1 0
                                       complexityAdjustment' <- Gtk.adjustmentNew 50 0 100 1 1 0
                                       Gtk.toggleButtonSetActive withTimeRadioButton True
                                       return (timeAdjustment', complexityAdjustment')
       WithComplexity complexity -> do timeAdjustment' <- Gtk.adjustmentNew 30 0 1000000 0.1 0.1 0
                                       complexityAdjustment' <- Gtk.adjustmentNew (fromIntegral complexity) 0 100 1 1 0
                                       Gtk.toggleButtonSetActive withComplexityRadioButton True
                                       return (timeAdjustment', complexityAdjustment')
     withTimeSpinButton <- Gtk.spinButtonNew timeAdjustment 0 1
     withComplexitySpinButton <- Gtk.spinButtonNew complexityAdjustment 0 0
     -- Set properties.
     preferencesDialog `Gtk.set` [Gtk.windowTitle := "Preferences"]
     Gtk.frameSetLabel gameFrame "Game"
     Gtk.entrySetText gameNameEntry (gameName startSettings)
     Gtk.frameSetLabel playersFrame "Players"
     Gtk.entrySetText redNameEntry (redName startSettings)
     Gtk.entrySetText blackNameEntry (blackName startSettings)
     Gtk.colorButtonSetColor redColorButton $ rgbToGtkColor $ redColor startSettings
     Gtk.colorButtonSetColor blackColorButton $ rgbToGtkColor $ blackColor startSettings
     Gtk.frameSetLabel otherFrame "Other"
     Gtk.colorButtonSetColor backgroundColorButton $ rgbToGtkColor $ backgroundColor startSettings
     Gtk.colorButtonSetColor gridColorButton $ rgbToGtkColor $ gridColor startSettings
     Gtk.toggleButtonSetActive horizontalReflectionCheckButton (horizontalReflection startSettings)
     Gtk.toggleButtonSetActive verticalReflectionCheckButton (verticalReflection startSettings)
     Gtk.frameSetLabel aiFrame "AI"
     Gtk.toggleButtonSetActive aiPresentCheckButton (aiPresent startSettings)
     when (aiPath startSettings /= "") $ void $ Gtk.fileChooserSetFilename aiPathFileChooserButton (aiPath startSettings)
     Gtk.toggleButtonSetActive aiRespondentCheckButton (aiRespondent startSettings)
     Gtk.frameSetLabel genMoveTypeFrame "Move generation type"
     -- Set hierarchy.
     Gtk.dialogAddActionWidget preferencesDialog applyButton Gtk.ResponseApply
     Gtk.dialogAddActionWidget preferencesDialog okButton Gtk.ResponseOk
     Gtk.dialogAddActionWidget preferencesDialog cancelButton Gtk.ResponseCancel
     Gtk.containerAdd preferencesDialogContent table
     Gtk.tableAttachDefaults table gameFrame 0 1 0 1
     Gtk.containerAdd gameFrame gameFrameTable
     Gtk.tableAttachDefaults gameFrameTable gameNameLabel 0 1 0 1
     Gtk.tableAttachDefaults gameFrameTable gameNameEntry 1 2 0 1
     Gtk.tableAttachDefaults gameFrameTable fieldWidthLabel 0 1 1 2
     Gtk.tableAttachDefaults gameFrameTable fieldWidthSpinButton 1 2 1 2
     Gtk.tableAttachDefaults gameFrameTable fieldHeightLabel 0 1 2 3
     Gtk.tableAttachDefaults gameFrameTable fieldHeightSpinButton 1 2 2 3
     Gtk.tableAttachDefaults table playersFrame 1 2 0 1
     Gtk.containerAdd playersFrame playersFrameTable
     Gtk.tableAttachDefaults playersFrameTable redNameLabel 0 1 0 1
     Gtk.tableAttachDefaults playersFrameTable redNameEntry 1 2 0 1
     Gtk.tableAttachDefaults playersFrameTable blackNameLabel 0 1 1 2
     Gtk.tableAttachDefaults playersFrameTable blackNameEntry 1 2 1 2
     Gtk.tableAttachDefaults playersFrameTable redColorLabel 0 1 2 3
     Gtk.tableAttachDefaults playersFrameTable redColorButton 1 2 2 3
     Gtk.tableAttachDefaults playersFrameTable blackColorLabel 0 1 3 4
     Gtk.tableAttachDefaults playersFrameTable blackColorButton 1 2 3 4
     Gtk.tableAttachDefaults table otherFrame 2 3 0 1
     Gtk.containerAdd otherFrame otherFrameTable
     Gtk.tableAttachDefaults otherFrameTable backgroundColorLabel 0 1 0 1
     Gtk.tableAttachDefaults otherFrameTable backgroundColorButton 1 2 0 1
     Gtk.tableAttachDefaults otherFrameTable gridColorLabel 0 1 1 2
     Gtk.tableAttachDefaults otherFrameTable gridColorButton 1 2 1 2
     Gtk.tableAttachDefaults otherFrameTable fillingAlphaLabel 0 1 2 3
     Gtk.tableAttachDefaults otherFrameTable fillingAlphaSpinButton 1 2 2 3
     Gtk.tableAttachDefaults otherFrameTable horizontalReflectionCheckButton 0 2 3 4
     Gtk.tableAttachDefaults otherFrameTable verticalReflectionCheckButton 0 2 4 5
     Gtk.tableAttachDefaults table aiFrame 3 4 0 1
     Gtk.containerAdd aiFrame aiFrameTable
     Gtk.tableAttachDefaults aiFrameTable aiPresentCheckButton 0 1 0 1
     Gtk.tableAttachDefaults aiFrameTable aiPathFileChooserButton 0 1 1 2
     Gtk.tableAttachDefaults aiFrameTable aiRespondentCheckButton 0 1 2 3
     Gtk.tableAttachDefaults aiFrameTable genMoveTypeFrame 0 1 3 4
     Gtk.containerAdd genMoveTypeFrame genMoveTypeFrameTable
     Gtk.tableAttachDefaults genMoveTypeFrameTable simpleRadioButton 0 1 0 1
     Gtk.tableAttachDefaults genMoveTypeFrameTable withTimeRadioButton 0 1 1 2
     Gtk.tableAttachDefaults genMoveTypeFrameTable withTimeSpinButton 1 2 1 2
     Gtk.tableAttachDefaults genMoveTypeFrameTable withComplexityRadioButton 0 1 2 3
     Gtk.tableAttachDefaults genMoveTypeFrameTable withComplexitySpinButton 1 2 2 3
     -- Return dialog.
     return PreferencesDialog { pdDialog = preferencesDialog,
                                pdGameNameEntry = gameNameEntry,
                                pdFieldWidthSpinButton = fieldWidthSpinButton,
                                pdFieldHeightSpinButton = fieldHeightSpinButton,
                                pdRedNameEntry = redNameEntry,
                                pdBlackNameEntry = blackNameEntry,
                                pdRedColorButton = redColorButton,
                                pdBlackColorButton = blackColorButton,
                                pdBackgroundColorButton = backgroundColorButton,
                                pdGridColorButton = gridColorButton,
                                pdFillingAlphaSpinButton = fillingAlphaSpinButton,
                                pdHorizontalReflectionCheckButton = horizontalReflectionCheckButton,
                                pdVerticalReflectionCheckButton = verticalReflectionCheckButton,
                                pdAiPresentCheckButton = aiPresentCheckButton,
                                pdAiPathFileChooserButton = aiPathFileChooserButton,
                                pdAiRespondentCheckButton = aiRespondentCheckButton,
                                pdSimpleRadioButton = simpleRadioButton,
                                pdWithTimeRadioButton = withTimeRadioButton,
                                pdWithTimeSpinButton = withTimeSpinButton,
                                pdWithComplexityRadioButton = withComplexityRadioButton,
                                pdWithComplexitySpinButton = withComplexitySpinButton }

runPreferencesDialog :: Settings -> PreferencesDialog -> (Settings -> IO ()) -> IO ()
runPreferencesDialog startSettings preferencesDialog f =
  do Gtk.widgetShowAll $ pdDialog preferencesDialog
     response <- Gtk.dialogRun $ pdDialog preferencesDialog
     case response of
       Gtk.ResponseDeleteEvent -> Gtk.widgetDestroy $ pdDialog preferencesDialog
       Gtk.ResponseApply       -> do settings <- getSettings startSettings preferencesDialog
                                     f settings
                                     runPreferencesDialog startSettings preferencesDialog f
       Gtk.ResponseOk          -> do settings <- getSettings startSettings preferencesDialog
                                     f settings
                                     Gtk.widgetDestroy $ pdDialog preferencesDialog
       Gtk.ResponseCancel      -> Gtk.widgetDestroy $ pdDialog preferencesDialog
       _                       -> error $ "preferencesDialog: unexpected response: " ++ show response

gameTabNew :: IO GameTab
gameTabNew =
  do table <- Gtk.tableNew 2 1 False
     drawingArea <- Gtk.drawingAreaNew
     coordLabel <- Gtk.labelNew (Nothing :: Maybe [Char])
     Gtk.tableAttachDefaults table drawingArea 0 1 0 1
     Gtk.tableAttach table coordLabel 0 1 1 2 [] [] 1 1
     return GameTab { gtWidget = Gtk.toWidget table,
                      gtDrawingArea = drawingArea,
                      gtCoordLabel = coordLabel }

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

listenGameTab :: GameWithBot -> GameTab -> IO ()
listenGameTab gwb gameTab =
  do let withPos f =
           do (x, y) <- Gtk.eventCoordinates
              liftIO $ do game <- get (gwbGame gwb)
                          width <- liftM fromIntegral $ liftIO $ Gtk.widgetGetAllocatedWidth $ gtDrawingArea gameTab
                          height <- liftM fromIntegral $ liftIO $ Gtk.widgetGetAllocatedHeight $ gtDrawingArea gameTab
                          let fields = gameFields game
                              headField = head fields
                              gameFieldWidth = fieldWidth headField
                              gameFieldHeight = fieldHeight headField
                              (posX, posY) = toGamePos game width height (x, y)
                          when (posX >= 0 && posY >= 0 && posX < gameFieldWidth && posY < gameFieldHeight) $ f (posX, posY)
     gtDrawingArea gameTab `Gtk.on` Gtk.draw $
       do game <- liftIO $ get (gwbGame gwb)
          width <- liftIO $ Gtk.widgetGetAllocatedWidth $ gtDrawingArea gameTab
          height <- liftIO $ Gtk.widgetGetAllocatedHeight $ gtDrawingArea gameTab
          draw game (fromIntegral width) (fromIntegral height)
     gtDrawingArea gameTab `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $
        do Gtk.LeftButton <- Gtk.eventButton
           withPos $ \pos -> do putGWBPoint pos gwb
                                Gtk.widgetQueueDraw $ gtDrawingArea gameTab
     Gtk.widgetAddEvents (gtDrawingArea gameTab) [Gtk.PointerMotionMask]
     gtDrawingArea gameTab `Gtk.on` Gtk.motionNotifyEvent $
       do withPos $ \(posX, posY) -> Gtk.labelSetText (gtCoordLabel gameTab) $ show (posX + 1) ++ ":" ++ show (posY + 1)
          return False
     return ()

mainWindowNew :: Gtk.Pixbuf -> IO MainWindow
mainWindowNew logo =
  do -- Create widgets.
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
     reflectHorizontallyMenuItem <- Gtk.menuItemNewWithLabel "Reflect horizontally"
     reflectVerticallyMenuItem <- Gtk.menuItemNewWithLabel "Reflect vertically"
     editSeparatorMenuItem <- Gtk.separatorMenuItemNew
     preferencesImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockPreferences
     helpImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockHelp
     helpMenu <- Gtk.menuNew
     aboutImageMenuItem <- Gtk.imageMenuItemNewFromStock Gtk.stockAbout
     notebook <- Gtk.notebookNew
     -- Set properties.
     Gtk.windowSetDefaultSize mainWindow 800 600
     mainWindow `Gtk.set` [Gtk.windowTitle := "Missile"]
     mainWindow `Gtk.set` [Gtk.windowIcon := Just logo]
     -- Set hierarchy.
     mainWindow `Gtk.set` [Gtk.containerChild := vbox]
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
     Gtk.containerAdd editMenu reflectHorizontallyMenuItem
     Gtk.containerAdd editMenu reflectVerticallyMenuItem
     Gtk.containerAdd editMenu editSeparatorMenuItem
     Gtk.containerAdd editMenu preferencesImageMenuItem
     Gtk.containerAdd menuBar helpImageMenuItem
     helpImageMenuItem `Gtk.set` [Gtk.menuItemSubmenu := helpMenu]
     Gtk.containerAdd helpMenu aboutImageMenuItem
     Gtk.containerAdd vbox notebook
     -- Return window.
     return MainWindow { mwWindow = mainWindow,
                         mwNewImageMenuItem = newImageMenuItem,
                         mwOpenImageMenuItem = openImageMenuItem,
                         mwSaveImageMenuItem = saveImageMenuItem,
                         mwCloseImageMenuItem = closeImageMenuItem,
                         mwQuitImageMenuItem = quitImageMenuItem,
                         mwUndoImageMenuItem = undoImageMenuItem,
                         mwReflectHorizontallyMenuItem = reflectHorizontallyMenuItem,
                         mwReflectVerticallyMenuItem = reflectVerticallyMenuItem,
                         mwPreferencesImageMenuItem = preferencesImageMenuItem,
                         mwAboutImageMenuItem = aboutImageMenuItem,
                         mwNotebook = notebook }

listenMainWindow :: IORef Settings -> IORef (IntMap.IntMap (GameTab, GameWithBot)) -> MainWindow -> Gtk.Pixbuf -> String -> IO ()
listenMainWindow globalSettingsRef tabsRef mainWindow logo license =
  let onExit =
        do tabs <- get tabsRef
           mapM_ (killGWBBot . snd) $ IntMap.elems tabs
           globalSettings <- get globalSettingsRef
           writeSettings globalSettings "settings.cfg"
           Gtk.mainQuit
      botErrorAlert game window =
        do messageDialog <- Gtk.messageDialogNew (Just window) [] Gtk.MessageError Gtk.ButtonsOk $ gameName (gameSettings game) ++ ": Bot made a mistake. It is killed."
           Gtk.dialogRun messageDialog
           Gtk.widgetDestroy messageDialog
      savingErrorAkert window =
        do messageDialog <- Gtk.messageDialogNew (Just window) [] Gtk.MessageError Gtk.ButtonsOk "Error: not saved."
           Gtk.dialogRun messageDialog
           Gtk.widgetDestroy messageDialog
      createGameTab notebook gwb =
        do game <- get (gwbGame gwb)
           gameTab <- gameTabNew
           let gwb' = gwb { gwbUpdated = Gtk.postGUIAsync $ Gtk.widgetQueueDraw $ gtDrawingArea gameTab }
           listenGameTab gwb' gameTab
           pageIndex <- Gtk.notebookAppendPage notebook (gtWidget gameTab) (gameName $ gameSettings game)
           modifyIORef tabsRef $ IntMap.insert pageIndex (gameTab, gwb')
           Gtk.widgetShowAll notebook
           Gtk.notebookSetCurrentPage notebook pageIndex
  in do mwWindow mainWindow `Gtk.on` Gtk.deleteEvent $ liftIO $ onExit >> return False
        mwQuitImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO onExit
        mwNewImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do globalSettings <- get globalSettingsRef
             preferencesDialog <- preferencesDialogNew globalSettings
             runPreferencesDialog globalSettings preferencesDialog $ \settings ->
               gameWithBot (emptyGame settings) (Gtk.postGUIAsync $ botErrorAlert (emptyGame settings) (mwWindow mainWindow)) >>= createGameTab (mwNotebook mainWindow)
        mwCloseImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do pageNum <- Gtk.notebookGetCurrentPage (mwNotebook mainWindow)
             when (pageNum /= -1) $
               do tabs <- get tabsRef
                  let (_, gwb) = tabs IntMap.! pageNum
                  killGWBBot gwb
                  Gtk.notebookRemovePage (mwNotebook mainWindow) pageNum
                  modifyIORef tabsRef $ IntMap.delete pageNum
        mwUndoImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do pageNum <- Gtk.notebookGetCurrentPage (mwNotebook mainWindow)
             when (pageNum /= -1) $
               do tabs <- get tabsRef
                  let (gameTab, gwb) = tabs IntMap.! pageNum
                  backGWB gwb
                  Gtk.widgetQueueDraw $ gtDrawingArea gameTab
        mwReflectHorizontallyMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do pageNum <- Gtk.notebookGetCurrentPage (mwNotebook mainWindow)
             when (pageNum /= -1) $
               do tabs <- get tabsRef
                  let (gameTab, gwb) = tabs IntMap.! pageNum
                  reflectHorizontallyGWB gwb
                  Gtk.widgetQueueDraw $ gtDrawingArea gameTab
        mwReflectVerticallyMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do pageNum <- Gtk.notebookGetCurrentPage (mwNotebook mainWindow)
             when (pageNum /= -1) $
               do tabs <- get tabsRef
                  let (gameTab, gwb) = tabs IntMap.! pageNum
                  reflectVerticallyGWB gwb
                  Gtk.widgetQueueDraw $ gtDrawingArea gameTab
        mwOpenImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do fileChooser <- Gtk.fileChooserDialogNew Nothing (Just (mwWindow mainWindow)) Gtk.FileChooserActionOpen [("Cancel", Gtk.ResponseCancel), ("OK", Gtk.ResponseOk)]
             fileChooser `Gtk.set` [Gtk.windowTitle := "Choose save of game"]
             xtFilter <- Gtk.fileFilterNew
             Gtk.fileFilterAddPattern xtFilter "*.sav"
             Gtk.fileFilterSetName xtFilter "PointsXT"
             Gtk.fileChooserAddFilter fileChooser xtFilter
             allFilter <- Gtk.fileFilterNew
             Gtk.fileFilterAddPattern allFilter "*"
             Gtk.fileFilterSetName allFilter "All files"
             Gtk.fileChooserAddFilter fileChooser allFilter
             response <- Gtk.dialogRun fileChooser
             case response of
               Gtk.ResponseDeleteEvent -> return ()
               Gtk.ResponseCancel      -> return ()
               Gtk.ResponseOk          -> do maybeFileName <- liftM (fmap decodeString) $ Gtk.fileChooserGetFilename fileChooser
                                             case maybeFileName of
                                               Just fileName ->
                                                 do globalSettings <- get globalSettingsRef
                                                    maybeGame <- XT.load fileName globalSettings
                                                    case maybeGame of
                                                      Just game -> gameWithBot game (Gtk.postGUIAsync $ botErrorAlert game (mwWindow mainWindow)) >>= createGameTab (mwNotebook mainWindow)
                                                      Nothing   -> return ()
                                               Nothing       -> return ()
               _                       -> error $ "fileChooser: unexpected response: " ++ show response
             Gtk.widgetDestroy fileChooser
        mwSaveImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do pageNum <- Gtk.notebookGetCurrentPage (mwNotebook mainWindow)
             when (pageNum /= -1) $
               do fileChooser <- Gtk.fileChooserDialogNew Nothing (Just (mwWindow mainWindow)) Gtk.FileChooserActionSave [("Cancel", Gtk.ResponseCancel), ("OK", Gtk.ResponseOk)]
                  Gtk.fileChooserSetDoOverwriteConfirmation fileChooser True
                  xtFilter <- Gtk.fileFilterNew
                  Gtk.fileFilterAddPattern xtFilter "*.sav"
                  Gtk.fileFilterSetName xtFilter "PointsXT"
                  Gtk.fileChooserAddFilter fileChooser xtFilter
                  response <- Gtk.dialogRun fileChooser
                  case response of
                    Gtk.ResponseDeleteEvent -> return ()
                    Gtk.ResponseCancel      -> return ()
                    Gtk.ResponseOk          -> do maybeFileName <- liftM (fmap decodeString) $ Gtk.fileChooserGetFilename fileChooser
                                                  case maybeFileName of
                                                    Just fileName -> do tabs <- get tabsRef
                                                                        let (_, gwb) = tabs IntMap.! pageNum
                                                                        game <- get (gwbGame gwb)
                                                                        saveResult <- XT.save fileName game
                                                                        unless saveResult $ savingErrorAkert $ mwWindow mainWindow
                                                    Nothing       -> savingErrorAkert $ mwWindow mainWindow
                    _                       -> error $ "fileChooser: unexpected response: " ++ show response
                  Gtk.widgetDestroy fileChooser
        mwPreferencesImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do pageNum <- Gtk.notebookGetCurrentPage (mwNotebook mainWindow)
             if pageNum /= -1
               then do tabs <- get tabsRef
                       let (gameTab, gwb) = tabs IntMap.! pageNum
                       game <- get (gwbGame gwb)
                       let settings = gameSettings game
                       preferencesDialog <- preferencesDialogNew settings
                       runPreferencesDialog settings preferencesDialog $ \newSettings ->
                         do updateGWBSettings gwb newSettings
                            Gtk.notebookSetTabLabelText (mwNotebook mainWindow) (gtWidget gameTab) (gameName newSettings)
                            game' <- get (gwbGame gwb)
                            modifyIORef tabsRef $ IntMap.insert pageNum (gameTab, gwb { gwbBotError = botErrorAlert game' (mwWindow mainWindow) })
                            return ()
               else do globalSettings <- get globalSettingsRef
                       preferencesDialog <- preferencesDialogNew globalSettings
                       runPreferencesDialog globalSettings preferencesDialog $ \newSettings ->
                         globalSettingsRef $= newSettings
        mwAboutImageMenuItem mainWindow `Gtk.on` Gtk.menuItemActivated $ liftIO $
          do aboutDialog <- Gtk.aboutDialogNew
             aboutDialog `Gtk.set` [Gtk.aboutDialogProgramName := "Missile"]
             aboutDialog `Gtk.set` [Gtk.aboutDialogVersion := "3.0.0"]
             aboutDialog `Gtk.set` [Gtk.aboutDialogLicense := Just license]
             aboutDialog `Gtk.set` [Gtk.aboutDialogWebsite := "https://gitorious.org/opai/missile"]
             aboutDialog `Gtk.set` [Gtk.aboutDialogAuthors := ["Kurnevsky Evgeny"]]
             aboutDialog `Gtk.set` [Gtk.aboutDialogLogo := Just logo]
             Gtk.dialogRun aboutDialog
             Gtk.widgetDestroy aboutDialog
        return ()

main :: IO ()
main =
  do Gtk.initGUI
     globalSettingsRef <- readSettings "settings.cfg" >>= newIORef
     tabsRef <- newIORef IntMap.empty
     logo <- Gtk.pixbufNewFromFile "Logo.png"
     license <- readFile "LICENSE"
     mainWindow <- mainWindowNew logo
     listenMainWindow globalSettingsRef tabsRef mainWindow logo license
     Gtk.widgetShowAll (mwWindow mainWindow)
     Gtk.mainGUI
