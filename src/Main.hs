module Main (main) where

import qualified Graphics.UI.Gtk as Gtk
--import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import qualified Graphics.UI.Gtk.Builder as GtkBuilder
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.Rendering.OpenGL as GL
import Data.Colour.RGBSpace as Colour
import Data.Colour.SRGB as SRGB
import qualified Data.IntMap as IntMap
import Codec.Binary.UTF8.String
import Data.Maybe
import Player
import Field
import Settings
import Game
import GameWithBot
import qualified FileFormats.XT as XT
import Auxiliary

fieldLeft :: GLfloat
fieldLeft = -1

fieldRight :: GLfloat
fieldRight = 1

fieldBottom :: GLfloat
fieldBottom = -1

fieldTop :: GLfloat
fieldTop = 1

fromPosX :: Settings -> GLfloat -> Int -> GLfloat
fromPosX settings cellWidth x = let x' = (fromIntegral x + 0.5) * cellWidth
                                in if horizontalReflection settings
                                   then fieldLeft + (fieldRight - fieldLeft) - x'
                                   else fieldLeft + x'

fromPosY :: Settings -> GLfloat -> Int -> GLfloat
fromPosY settings cellHeight y = let y' = (fromIntegral y + 0.5) * cellHeight
                                 in if verticalReflection settings
                                    then fieldBottom + (fieldTop - fieldBottom) - y'
                                    else fieldBottom + y'

fromPos :: Settings -> GLfloat -> GLfloat -> Pos -> (GLfloat, GLfloat)
fromPos settings cellWidth cellHeight (x, y) = (fromPosX settings cellWidth x, fromPosY settings cellHeight y)

toPos :: (RealFrac a) => Settings -> a -> a -> a -> a -> Pos
toPos settings cellWidth cellHeight x y = let posX = floor $ x / cellWidth
                                              posY = floor $ y / cellHeight
                                              posX' = if horizontalReflection settings
                                                      then gameWidth settings - posX - 1
                                                      else posX
                                              posY' = if verticalReflection settings
                                                      then gameHeight settings - posY - 1
                                                      else posY
                                              pos = (posX', posY')
                                          in pos

makeVertex :: [(GLfloat, GLfloat)] -> IO ()
makeVertex = mapM_ (\(x, y) -> vertex $ Vertex2 x y)

ellipse :: (GLfloat, GLfloat) -> GLfloat -> GLfloat -> Int -> [(GLfloat, GLfloat)]
ellipse (x, y) radiusX radiusY divs = map toPoint angles
         where arc = 2.0 * pi / fromIntegral divs
               toPoint a = (x + cos a * radiusX, y + sin a * radiusY)
               angles = map ((* arc) . fromIntegral) [0..divs]

complexPolygon :: [Vertex2 GLfloat] -> ComplexPolygon GLfloat
complexPolygon points' = ComplexPolygon [ComplexContour $ map (\(Vertex2 x y) -> AnnotatedVertex (Vertex3 (realToFrac x) (realToFrac y) 0) 0) points']

renderPrimitivePart :: Primitive GLfloat -> IO ()
renderPrimitivePart (Primitive primitiveMode vertices) = renderPrimitive primitiveMode $ mapM_ (\(AnnotatedVertex plainVertex _) -> vertex plainVertex) vertices

nonConvexPoligon :: [Vertex2 GLfloat] -> IO ()
nonConvexPoligon poligon = do SimplePolygon primitiveParts <- tessellate TessWindingPositive 0 (Normal3 0 0 0) (\_ _ -> 0) $ complexPolygon poligon
                              mapM_ renderPrimitivePart primitiveParts

rgbToColor4 :: RGB Double -> Double -> Color4 GLfloat
rgbToColor4 rgb a = Color4 (realToFrac $ channelRed rgb) (realToFrac $ channelGreen rgb) (realToFrac $ channelBlue rgb) (realToFrac a)

rgbToColor3 :: RGB Double -> Color3 GLfloat
rgbToColor3 rgb = Color3 (realToFrac $ channelRed rgb) (realToFrac $ channelGreen rgb) (realToFrac $ channelBlue rgb)

gtkColorToRgb :: Gtk.Color -> RGB Double
gtkColorToRgb (Gtk.Color r g b) = toSRGB $ sRGBBounded r g b

rgbToGtkColor :: RGB Double -> Gtk.Color
rgbToGtkColor (Colour.RGB r g b) = let Colour.RGB r' g' b' = toSRGBBounded $ sRGB r g b
                                   in Gtk.Color r' g' b'

display :: Game -> IO ()
display game =
        do let fields = gameFields game
               headField = head fields
               settings = gameSettings game
               cellWidth = (fieldRight - fieldLeft) / fromIntegral (fieldWidth headField)
               cellHeight = (fieldTop - fieldBottom) / fromIntegral (fieldHeight headField)
               fromPosX' = fromPosX settings cellWidth
               fromPosY' = fromPosY settings cellHeight
               fromPos' = fromPos settings cellWidth cellHeight
               verticalLines = map (\i -> let x = fromPosX' i
                                          in [(x, fieldBottom), (x, fieldTop)]) [0..(fieldWidth headField - 1)]
               horizontalLines = map (\i -> let y = fromPosY' i
                                            in [(fieldLeft, y), (fieldRight, y)]) [0..(fieldHeight headField - 1)]
               redColor3 = rgbToColor3 (redColor settings)
               blackColor3 = rgbToColor3 (blackColor settings)
               redColor4 = rgbToColor4 (redColor settings) (fillingAlpha settings)
               blackColor4 = rgbToColor4 (blackColor settings) (fillingAlpha settings)
           --Rendering background.
           clearColor $= rgbToColor4 (backgroundColor $ gameSettings game) 0
           GL.clear [GL.DepthBuffer, GL.ColorBuffer]
           --Rendering grig.
           lineWidth $= fromIntegral (gridThickness settings)
           currentColor $= rgbToColor4 (gridColor settings) 1
           renderPrimitive Lines $ makeVertex $ concat (verticalLines ++ horizontalLines)
           --Rendering points.
           --First way.
           --mapM_ (renderPrimitive Polygon) $ map (\((x, y), player) -> do
           --     color $ if player == Player.Red then redColor3 else blackColor3
           --     makeVertex $ ellipse (fromPosX' x, fromPosY' y)
           --                          ((realToFrac $ pointRadius settings) / (fromIntegral $ fieldWidth headField) * 0.4)
           --                          ((realToFrac $ pointRadius settings) / (fromIntegral $ fieldHeight headField) * 0.4)
           --                          (pointDetailling settings)) $ moves headField
           --Second way.
           --pointSmooth $= Enabled
           --pointSize $= 20
           --mapM_ (renderPrimitive Points) $ map (\((x, y), player) -> do
           --     color $ if player == Player.Red then redColor3 else blackColor3
           --     makeVertex [(fromPosX' x, fromPosY' y)]) $ moves headField
           --pointSmooth $= Disabled
           --Third way.
           lineWidth $= 2
           lineSmooth $= Enabled
           mapM_ (renderPrimitive Polygon . (\((x, y), player) -> do
                color $ if player == Player.Red then redColor3 else blackColor3
                makeVertex $ ellipse (fromPosX' x, fromPosY' y)
                                     (realToFrac (pointRadius settings) / fromIntegral (fieldWidth headField) * 0.3)
                                     (realToFrac (pointRadius settings) / fromIntegral (fieldHeight headField) * 0.3)
                                     (pointDetailling settings))) $ moves headField
           renderPrimitive Lines $ mapM_ (\((x, y), player) -> do
                   color $ if player == Player.Red then redColor3 else blackColor3
                   let ellipsePoints = ellipse (fromPosX' x, fromPosY' y)
                                               (realToFrac (pointRadius settings) / fromIntegral (fieldWidth headField) * 0.3)
                                               (realToFrac (pointRadius settings) / fromIntegral (fieldHeight headField) * 0.3)
                                               (pointDetailling settings)
                   makeVertex $ foldl (\acc (a, b) -> a : b : acc) [] $ zip ellipsePoints $ leftShift1 ellipsePoints) $ moves headField
           --Rendering last point.
           unless (null $ moves headField) $
             renderPrimitive Lines $ (\((x, y), player) -> do
               color $ if player == Player.Red then redColor3 else blackColor3
               let ellipsePoints = ellipse (fromPosX' x, fromPosY' y)
                                           (realToFrac (pointRadius settings) / fromIntegral (fieldWidth headField) * 0.6)
                                           (realToFrac (pointRadius settings) / fromIntegral (fieldHeight headField) * 0.6)
                                           (pointDetailling settings)
               makeVertex $ foldl (\acc (a, b) -> a : b : acc) [] $ zip ellipsePoints $ leftShift1 ellipsePoints) $ head $ moves headField
           lineSmooth $= Disabled
           --Rendering little surrounds.
           when (fullFill settings) $
             mapM_ (\(field, (pos, player)) -> do
                               currentColor $= if player == Player.Red then redColor4 else blackColor4
                               if playersPoint field (s pos) player && playersPoint field (e pos) player
                                 then renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ s pos, fromPos' $ e pos]
                                 else do when (playersPoint field (s pos) player && playersPoint field (se pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ s pos, fromPos' $ se pos]
                                         when (playersPoint field (e pos) player && playersPoint field (se pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ e pos, fromPos' $ se pos]
                               if playersPoint field (e pos) player && playersPoint field (n pos) player
                                 then renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ e pos, fromPos' $ n pos]
                                 else do when (playersPoint field (e pos) player && playersPoint field (ne pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ e pos, fromPos' $ ne pos]
                                         when (playersPoint field (n pos) player && playersPoint field (ne pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ n pos, fromPos' $ ne pos]
                               if playersPoint field (n pos) player && playersPoint field (w pos) player
                                 then renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ n pos, fromPos' $ w pos]
                                 else do when (playersPoint field (n pos) player && playersPoint field (nw pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ n pos, fromPos' $ nw pos]
                                         when (playersPoint field (w pos) player && playersPoint field (nw pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ w pos, fromPos' $ nw pos]
                               if playersPoint field (w pos) player && playersPoint field (s pos) player
                                 then renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ w pos, fromPos' $ s pos]
                                 else do when (playersPoint field (w pos) player && playersPoint field (sw pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ w pos, fromPos' $ sw pos]
                                         when (playersPoint field (s pos) player && playersPoint field (sw pos) player) $
                                           renderPrimitive Polygon $ makeVertex [fromPos' pos, fromPos' $ s pos, fromPos' $ sw pos]
                               ) $ zip (reverse fields) (map (head . moves) $ tail $ reverse fields)
           --Rendering surrounds.
           mapM_ (\(chain, player) -> do currentColor $= if player == Player.Red then redColor4 else blackColor4
                                         nonConvexPoligon $ map (\(x, y) -> Vertex2 (fromPosX settings cellWidth x) (fromPosY settings cellHeight y)) chain
                                         ) $ filter (not . null . fst) $ map lastSurroundChain $ reverse fields

updateCanvas :: GtkGL.GLDrawingArea -> Game -> IO ()
updateCanvas canvas game = GtkGL.withGLDrawingArea canvas $ \glwindow -> do display game
                                                                            flush
                                                                            GtkGL.glDrawableSwapBuffers glwindow

createCanvas :: IO GtkGL.GLDrawingArea
createCanvas =
        do -- We need a OpenGL frame buffer configuration to be able to create other OpenGL objects.
           glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                          GtkGL.GLModeDepth,
                                          GtkGL.GLModeDouble,
                                          GtkGL.GLModeMultiSample]
           -- Create an OpenGL drawing area widget
           canvas <- GtkGL.glDrawingAreaNew glconfig
           -- Initialise some GL setting just before the canvas first gets shown
           -- (We can't initialise these things earlier since the GL resources that
           -- we are using wouldn't heve been setup yet)
           canvas `Gtk.on` Gtk.realize $ GtkGL.withGLDrawingArea canvas $ \_ -> do
                blend $= Enabled
                blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
                shadeModel $= Flat
                --shadeModel $= Smooth
                hint LineSmooth $= Nicest
                --hint PointSmooth $= Nicest
                multisample $= Enabled
                loadIdentity
           return canvas

connectCanvas :: GtkGL.GLDrawingArea -> GameWithBot -> IO ()
connectCanvas canvas gwb =
        do canvas `Gtk.on` Gtk.exposeEvent $ Gtk.tryEvent $ liftIO $ do
                game <- get (gwbGame gwb)
                (width, height) <- Gtk.widgetGetSize canvas
                let gameFieldWidth = fieldWidth $ head $ gameFields game
                    gameFieldHeight = fieldHeight $ head $ gameFields game
                    width' = min width ((height * gameFieldWidth) `div` gameFieldHeight)
                    height' = min height ((width * gameFieldHeight) `div` gameFieldWidth)
                GL.viewport $= (Position (fromIntegral $ (width - width') `div` 2) (fromIntegral $ (height - height') `div` 2), Size (fromIntegral width') (fromIntegral height'))
                updateCanvas canvas game
           canvas `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do
                Gtk.LeftButton <- Gtk.eventButton
                (x, y) <- Gtk.eventCoordinates
                liftIO $ do game <- get (gwbGame gwb)
                            (width, height) <- Gtk.widgetGetSize canvas
                            let gameFieldWidth = fieldWidth $ head $ gameFields game
                                gameFieldHeight = fieldHeight $ head $ gameFields game
                                width' = min width ((height * gameFieldWidth) `div` gameFieldHeight)
                                height' = min height ((width * gameFieldHeight) `div` gameFieldWidth)
                                cellWidth = fromIntegral width' / fromIntegral gameFieldWidth
                                cellHeight = fromIntegral height' / fromIntegral gameFieldHeight
                                x' = x - fromIntegral ((width - width') `div` 2)
                                y' = fromIntegral height' - (y - fromIntegral ((height - height') `div` 2))
                                pos = toPos (gameSettings game) cellWidth cellHeight x' y'
                            putGWBPoint pos gwb
           return ()

data GameTab = GameTab { tabTable :: Gtk.Table,
                         canvasTable :: Gtk.Table }

getGameTab :: IO GameTab
getGameTab = do builder <- GtkBuilder.builderNew
                GtkBuilder.builderAddFromFile builder "interface/tab.glade"
                window <- GtkBuilder.builderGetObject builder Gtk.castToWindow "tabWindow"
                curTabTable <- GtkBuilder.builderGetObject builder Gtk.castToTable "tabTable"
                curCanvasTable <- GtkBuilder.builderGetObject builder Gtk.castToTable "canvasTable"
                Gtk.containerRemove window curTabTable
                Gtk.widgetDestroy window
                return GameTab { tabTable = curTabTable,
                                 canvasTable = curCanvasTable }

getAboutDialog :: IO Gtk.AboutDialog
getAboutDialog = do builder <- GtkBuilder.builderNew
                    GtkBuilder.builderAddFromFile builder "interface/about.glade"
                    GtkBuilder.builderGetObject builder Gtk.castToAboutDialog "aboutDialog"

data MainWindow = MainWindow { mainWindow :: Gtk.Window,
                               mainNotebook :: Gtk.Notebook,
                               exitMenuItem :: Gtk.ImageMenuItem,
                               createMenuItem :: Gtk.ImageMenuItem,
                               openMenuItem :: Gtk.ImageMenuItem,
                               saveMenuItem :: Gtk.ImageMenuItem,
                               closeMenuItem :: Gtk.ImageMenuItem,
                               undoMenuItem :: Gtk.ImageMenuItem,
                               settingsMenuItem :: Gtk.ImageMenuItem,
                               aboutMenuItem :: Gtk.ImageMenuItem }

getMainWindow :: IO MainWindow
getMainWindow = do builder <- GtkBuilder.builderNew
                   GtkBuilder.builderAddFromFile builder "interface/window.glade"
                   curWindow <- GtkBuilder.builderGetObject builder Gtk.castToWindow "mainWindow"
                   curMainNotebook <- GtkBuilder.builderGetObject builder Gtk.castToNotebook "mainNotebook"
                   curExitMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "exitMenuItem"
                   curCreateMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "createMenuItem"
                   curOpenMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "openMenuItem"
                   curSaveMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "saveMenuItem"
                   curCloseMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "closeMenuItem"
                   curUndoMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "undoMenuItem"
                   curSettingsMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "settingsMenuItem"
                   curAboutMenuItem <- GtkBuilder.builderGetObject builder Gtk.castToImageMenuItem "aboutMenuItem"
                   return MainWindow { mainWindow = curWindow,
                                       mainNotebook = curMainNotebook,
                                       exitMenuItem = curExitMenuItem,
                                       createMenuItem = curCreateMenuItem,
                                       openMenuItem = curOpenMenuItem,
                                       saveMenuItem = curSaveMenuItem,
                                       closeMenuItem = curCloseMenuItem,
                                       undoMenuItem = curUndoMenuItem,
                                       settingsMenuItem = curSettingsMenuItem,
                                       aboutMenuItem = curAboutMenuItem }

data SettingsWindow = SettingsWindow { settingsDialog :: Gtk.Dialog,
                                       fieldWidthSpinButton :: Gtk.SpinButton,
                                       fieldHeightSpinButton :: Gtk.SpinButton,
                                       gameNameEntry :: Gtk.Entry,
                                       redNameEntry :: Gtk.Entry,
                                       blackNameEntry :: Gtk.Entry,
                                       redColorButton :: Gtk.ColorButton,
                                       blackColorButton :: Gtk.ColorButton,
                                       backgroundColorButton :: Gtk.ColorButton,
                                       gridColorButton :: Gtk.ColorButton,
                                       fillingAlphaSpinButton :: Gtk.SpinButton,
                                       horizontalReflectionCheckButton :: Gtk.CheckButton,
                                       verticalReflectionCheckButton :: Gtk.CheckButton,
                                       aiPresentCheckButton :: Gtk.CheckButton,
                                       aiPathFileChooserButton :: Gtk.FileChooserButton,
                                       aiRespondentCheckButton :: Gtk.CheckButton,
                                       simpleRadioButton :: Gtk.RadioButton,
                                       withTimeRadioButton :: Gtk.RadioButton,
                                       withTimeSpinButton :: Gtk.SpinButton,
                                       withComplexityRadioButton :: Gtk.RadioButton,
                                       withComplexitySpinButton :: Gtk.SpinButton,
                                       applyButton :: Gtk.Button,
                                       okButton :: Gtk.Button,
                                       cancelButton :: Gtk.Button }

getSettingsWindow :: IO SettingsWindow
getSettingsWindow = do builder <- GtkBuilder.builderNew
                       GtkBuilder.builderAddFromFile builder "interface/settings.glade"
                       curSettingsDialog <- GtkBuilder.builderGetObject builder Gtk.castToDialog "settingsDialog"
                       curFieldWidthSpinButton <- GtkBuilder.builderGetObject builder Gtk.castToSpinButton "fieldWidthSpinButton"
                       curFieldHeightSpinButton <- GtkBuilder.builderGetObject builder Gtk.castToSpinButton "fieldHeightSpinButton"
                       curGameNameEntry <- GtkBuilder.builderGetObject builder Gtk.castToEntry "gameNameEntry"
                       curRedNameEntry <- GtkBuilder.builderGetObject builder Gtk.castToEntry "redNameEntry"
                       curBlackNameEntry <- GtkBuilder.builderGetObject builder Gtk.castToEntry "blackNameEntry"
                       curRedColorButton <- GtkBuilder.builderGetObject builder Gtk.castToColorButton "redColorButton"
                       curBlackColorButton <- GtkBuilder.builderGetObject builder Gtk.castToColorButton "blackColorButton"
                       curBackgroundColorButton <- GtkBuilder.builderGetObject builder Gtk.castToColorButton "backgroundColorButton"
                       curGridColorButton <- GtkBuilder.builderGetObject builder Gtk.castToColorButton "gridColorButton"
                       curFillingAlphaSpinButton <- GtkBuilder.builderGetObject builder Gtk.castToSpinButton "fillingAlphaSpinButton"
                       curHorizontalReflectionCheckButton <- GtkBuilder.builderGetObject builder Gtk.castToCheckButton "horizontalReflectionCheckButton"
                       curVerticalReflectionCheckButton <- GtkBuilder.builderGetObject builder Gtk.castToCheckButton "verticalReflectionCheckButton"
                       curAiPresentCheckButton <- GtkBuilder.builderGetObject builder Gtk.castToCheckButton "aiPresentCheckButton"
                       curAiPathFileChooserButton <- GtkBuilder.builderGetObject builder Gtk.castToFileChooserButton "aiPathFileChooserButton"
                       curAiRespondentCheckButton <- GtkBuilder.builderGetObject builder Gtk.castToCheckButton "aiRespondentCheckButton"
                       curSimpleRadioButton <- GtkBuilder.builderGetObject builder Gtk.castToRadioButton "simpleRadioButton"
                       curWithTimeRadioButton <- GtkBuilder.builderGetObject builder Gtk.castToRadioButton "withTimeRadioButton"
                       curWithTimeSpinButton <- GtkBuilder.builderGetObject builder Gtk.castToSpinButton "withTimeSpinButton"
                       curWithComplexityRadioButton <- GtkBuilder.builderGetObject builder Gtk.castToRadioButton "withComplexityRadioButton"
                       curWithComplexitySpinButton <- GtkBuilder.builderGetObject builder Gtk.castToSpinButton "withComplexitySpinButton"
                       curApplyButton <- GtkBuilder.builderGetObject builder Gtk.castToButton "applyButton"
                       curOkButton <- GtkBuilder.builderGetObject builder Gtk.castToButton "okButton"
                       curCancelButton <- GtkBuilder.builderGetObject builder Gtk.castToButton "cancelButton"
                       return SettingsWindow { settingsDialog = curSettingsDialog,
                                               fieldWidthSpinButton = curFieldWidthSpinButton,
                                               fieldHeightSpinButton = curFieldHeightSpinButton,
                                               gameNameEntry = curGameNameEntry,
                                               redNameEntry = curRedNameEntry,
                                               blackNameEntry = curBlackNameEntry,
                                               redColorButton = curRedColorButton,
                                               blackColorButton = curBlackColorButton,
                                               backgroundColorButton = curBackgroundColorButton,
                                               gridColorButton = curGridColorButton,
                                               fillingAlphaSpinButton = curFillingAlphaSpinButton,
                                               horizontalReflectionCheckButton = curHorizontalReflectionCheckButton,
                                               verticalReflectionCheckButton = curVerticalReflectionCheckButton,
                                               aiPresentCheckButton = curAiPresentCheckButton,
                                               aiPathFileChooserButton = curAiPathFileChooserButton,
                                               aiRespondentCheckButton = curAiRespondentCheckButton,
                                               simpleRadioButton = curSimpleRadioButton,
                                               withTimeRadioButton = curWithTimeRadioButton,
                                               withTimeSpinButton = curWithTimeSpinButton,
                                               withComplexityRadioButton = curWithComplexityRadioButton,
                                               withComplexitySpinButton = curWithComplexitySpinButton,
                                               applyButton = curApplyButton,
                                               okButton = curOkButton,
                                               cancelButton = curCancelButton }

createSettingsWindow :: Settings -> (Settings -> IO ()) -> IO Gtk.Dialog
createSettingsWindow startSettings f =
    do settingsWindow <- getSettingsWindow
       let getSettings = do curGameWidth <- liftM round $ Gtk.get (fieldWidthSpinButton settingsWindow) Gtk.spinButtonValue
                            curGameHeight <- liftM round $ Gtk.get (fieldHeightSpinButton settingsWindow) Gtk.spinButtonValue
                            curGameName <- liftM trim $ Gtk.entryGetText $ gameNameEntry settingsWindow
                            curRedName <- liftM trim $ Gtk.entryGetText $ redNameEntry settingsWindow
                            curBlackName <- liftM trim $ Gtk.entryGetText $ blackNameEntry settingsWindow
                            curRedColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor (redColorButton settingsWindow)
                            curBlackColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor (blackColorButton settingsWindow)
                            curBackgroundColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor (backgroundColorButton settingsWindow)
                            curFillingAlpha <- Gtk.get (fillingAlphaSpinButton settingsWindow) Gtk.spinButtonValue
                            curGridColor <- liftM gtkColorToRgb $ Gtk.colorButtonGetColor (gridColorButton settingsWindow)
                            curHorizontalReflection <- Gtk.toggleButtonGetActive (horizontalReflectionCheckButton settingsWindow)
                            curVerticalReflection <- Gtk.toggleButtonGetActive (verticalReflectionCheckButton settingsWindow)
                            curAiPresent <- Gtk.toggleButtonGetActive (aiPresentCheckButton settingsWindow)
                            curAiPathMaybe <- liftM (fmap decodeString) $ Gtk.fileChooserGetFilename (aiPathFileChooserButton settingsWindow)
                            curAiRespondent <- Gtk.toggleButtonGetActive (aiRespondentCheckButton settingsWindow)
                            curSimple <- Gtk.toggleButtonGetActive (simpleRadioButton settingsWindow)
                            curWithTime <- Gtk.toggleButtonGetActive (withTimeRadioButton settingsWindow)
                            curTime <- liftM (round . (* 1000)) $ Gtk.get (withTimeSpinButton settingsWindow) Gtk.spinButtonValue
                            curWithComplexity <- Gtk.toggleButtonGetActive (withComplexityRadioButton settingsWindow)
                            curComplexity <- liftM round $ Gtk.get (withComplexitySpinButton settingsWindow) Gtk.spinButtonValue
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
                                                   aiGenMoveType = if curSimple
                                                                   then Simple
                                                                   else if curWithTime
                                                                   then WithTime curTime
                                                                   else if curWithComplexity
                                                                   then WithComplexity curComplexity
                                                                   else error "Type of move generation not specified!" }
       fieldWidthAdjustment <- Gtk.adjustmentNew (fromIntegral $ gameWidth startSettings) 1 999 1 1 0
       Gtk.spinButtonConfigure (fieldWidthSpinButton settingsWindow) fieldWidthAdjustment 0 0
       fieldHeightAdjustment <- Gtk.adjustmentNew (fromIntegral $ gameHeight startSettings) 1 999 1 1 0
       Gtk.spinButtonConfigure (fieldHeightSpinButton settingsWindow) fieldHeightAdjustment 0 0
       Gtk.entrySetText (gameNameEntry settingsWindow) (gameName startSettings)
       Gtk.entrySetText (redNameEntry settingsWindow) (redName startSettings)
       Gtk.entrySetText (blackNameEntry settingsWindow) (blackName startSettings)
       Gtk.colorButtonSetColor (redColorButton settingsWindow) $ rgbToGtkColor $ redColor startSettings
       Gtk.colorButtonSetColor (blackColorButton settingsWindow) $ rgbToGtkColor $ blackColor startSettings
       Gtk.colorButtonSetColor (backgroundColorButton settingsWindow) $ rgbToGtkColor $ backgroundColor startSettings
       Gtk.colorButtonSetColor (gridColorButton settingsWindow) $ rgbToGtkColor $ gridColor startSettings
       fillingAlphaAdjustment <- Gtk.adjustmentNew (fillingAlpha startSettings) 0 1 0.01 0.01 0
       Gtk.spinButtonConfigure (fillingAlphaSpinButton settingsWindow) fillingAlphaAdjustment 0 2
       Gtk.toggleButtonSetActive (horizontalReflectionCheckButton settingsWindow) (horizontalReflection startSettings)
       Gtk.toggleButtonSetActive (verticalReflectionCheckButton settingsWindow) (verticalReflection startSettings)
       Gtk.toggleButtonSetActive (aiPresentCheckButton settingsWindow) (aiPresent startSettings)
       when (aiPath startSettings /= "") $
         void $ Gtk.fileChooserSetFilename (aiPathFileChooserButton settingsWindow) (aiPath startSettings)
       Gtk.toggleButtonSetActive (aiRespondentCheckButton settingsWindow) (aiRespondent startSettings)
       case aiGenMoveType startSettings of
         Simple                    -> do timeAdjustment <- Gtk.adjustmentNew 30 0 1000000 0.1 0.1 0
                                         Gtk.spinButtonConfigure (withTimeSpinButton settingsWindow) timeAdjustment 0 1
                                         complexityAdjustment <- Gtk.adjustmentNew 50 0 100 1 1 0
                                         Gtk.spinButtonConfigure (withComplexitySpinButton settingsWindow) complexityAdjustment 0 0
                                         Gtk.toggleButtonSetActive (simpleRadioButton settingsWindow) True
         WithTime time             -> do timeAdjustment <- Gtk.adjustmentNew (fromIntegral time / 1000) 0 1000000 0.1 0.1 0
                                         Gtk.spinButtonConfigure (withTimeSpinButton settingsWindow) timeAdjustment 0 1
                                         complexityAdjustment <- Gtk.adjustmentNew 50 0 100 1 1 0
                                         Gtk.spinButtonConfigure (withComplexitySpinButton settingsWindow) complexityAdjustment 0 0
                                         Gtk.toggleButtonSetActive (withTimeRadioButton settingsWindow) True
         WithComplexity complexity -> do timeAdjustment <- Gtk.adjustmentNew 30 0 1000000 0.1 0.1 0
                                         Gtk.spinButtonConfigure (withTimeSpinButton settingsWindow) timeAdjustment 0 1
                                         complexityAdjustment <- Gtk.adjustmentNew (fromIntegral complexity) 0 100 1 1 0
                                         Gtk.spinButtonConfigure (withComplexitySpinButton settingsWindow) complexityAdjustment 0 0
                                         Gtk.toggleButtonSetActive (withComplexityRadioButton settingsWindow) True
       cancelButton settingsWindow `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
           Gtk.widgetDestroy (settingsDialog settingsWindow)
           return False
       applyButton settingsWindow `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
           getSettings >>= f
           return False
       okButton settingsWindow `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
           getSettings >>= f
           Gtk.widgetDestroy (settingsDialog settingsWindow)
           return False
       return (settingsDialog settingsWindow)

main :: IO ()
main = do Gtk.initGUI
          GtkGL.initGL
          tabsRef <- newIORef IntMap.empty
          globalSettingsRef <- readSettings "settings.cfg" >>= newIORef
          window <- getMainWindow
          let botErrorAlert game = do
                  messageDialog <- Gtk.messageDialogNew (Just (mainWindow window)) [] Gtk.MessageError Gtk.ButtonsOk $ gameName (gameSettings game) ++ ": Bot made a mistake. It is killed."
                  Gtk.dialogRun messageDialog
                  Gtk.widgetDestroy messageDialog
              createGameTab gwb = do
                  game <- get (gwbGame gwb)
                  gameTab <- getGameTab
                  canvas <- createCanvas
                  Gtk.tableAttachDefaults (canvasTable gameTab) canvas 0 1 0 1
                  pageIndex <- Gtk.notebookAppendPage (mainNotebook window) (tabTable gameTab) (gameName $ gameSettings game)
                  let gwb' = gwb { gwbUpdated = get (gwbGame gwb') >>= (Gtk.postGUIAsync . updateCanvas canvas) }
                  connectCanvas canvas gwb'
                  modifyIORef tabsRef $ IntMap.insert pageIndex gwb'
                  Gtk.widgetShowAll (mainNotebook window)
                  Gtk.notebookSetCurrentPage (mainNotebook window) pageIndex
              onExit = do tabs <- get tabsRef
                          mapM_ killGWBBot (IntMap.elems tabs)
                          globalSettings <- get globalSettingsRef
                          writeSettings globalSettings "settings.cfg"
                          Gtk.mainQuit
          mainWindow window `Gtk.on` Gtk.deleteEvent $ liftIO $ onExit >> return False
          exitMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ onExit >> return False
          createMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
                globalSettings <- get globalSettingsRef
                curSettingsDialog <- createSettingsWindow globalSettings $ \settings ->
                    gameWithBot (emptyGame settings) (Gtk.postGUIAsync $ botErrorAlert $ emptyGame settings) >>= createGameTab
                Gtk.dialogRun curSettingsDialog
                return False
          closeMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
                pageNum <- Gtk.notebookGetCurrentPage (mainNotebook window)
                when (pageNum /= -1) $
                  do tabs <- get tabsRef
                     let gwb = tabs IntMap.! pageNum
                     killGWBBot gwb
                     Gtk.notebookRemovePage (mainNotebook window) pageNum
                     modifyIORef tabsRef $ IntMap.delete pageNum
                return False
          undoMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
                pageNum <- Gtk.notebookGetCurrentPage (mainNotebook window)
                when (pageNum /= -1) $
                  do tabs <- get tabsRef
                     let gwb = tabs IntMap.! pageNum
                     backGWB gwb
                return False
          settingsMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
                pageNum <- Gtk.notebookGetCurrentPage (mainNotebook window)
                if pageNum /= -1
                  then do Just page <- Gtk.notebookGetNthPage (mainNotebook window) pageNum
                          tabs <- get tabsRef
                          let gwb = tabs IntMap.! pageNum
                          game <- get (gwbGame gwb)
                          curSettingsDialog <- createSettingsWindow (gameSettings game) $ \newSettings -> do
                                updateGWBSettings gwb newSettings
                                Gtk.notebookSetTabLabelText (mainNotebook window) page (gameName newSettings)
                                game' <- get (gwbGame gwb)
                                modifyIORef tabsRef $ IntMap.insert pageNum gwb { gwbBotError = botErrorAlert game' }
                                return ()
                          Gtk.dialogRun curSettingsDialog
                  else do globalSettings <- get globalSettingsRef
                          curSettingsDialog <- createSettingsWindow globalSettings $ \settings ->
                            globalSettingsRef $= settings
                          Gtk.dialogRun curSettingsDialog
                return False
          openMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
                fileChooser <- Gtk.fileChooserDialogNew Nothing (Just (mainWindow window)) Gtk.FileChooserActionOpen [("Cancel", Gtk.ResponseCancel), ("OK", Gtk.ResponseOk)]
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
                  Gtk.ResponseCancel -> return ()
                  Gtk.ResponseOk     -> do maybeFileName <- liftM (fmap decodeString) $ Gtk.fileChooserGetFilename fileChooser
                                           case maybeFileName of
                                             Just fileName -> do globalSettings <- get globalSettingsRef
                                                                 maybeGame <- XT.load fileName globalSettings
                                                                 case maybeGame of
                                                                   Just game -> gameWithBot game (Gtk.postGUIAsync $ botErrorAlert game) >>= createGameTab
                                                                   Nothing   -> return ()
                                             Nothing       -> return ()
                  _                  -> error "fileChooser: uncknown response."
                Gtk.widgetDestroy fileChooser
                return False
          saveMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
                pageNum <- Gtk.notebookGetCurrentPage (mainNotebook window)
                when (pageNum /= -1) $
                  do fileChooser <- Gtk.fileChooserDialogNew Nothing (Just (mainWindow window)) Gtk.FileChooserActionSave [("Cancel", Gtk.ResponseCancel), ("OK", Gtk.ResponseOk)]
                     Gtk.fileChooserSetDoOverwriteConfirmation fileChooser True
                     xtFilter <- Gtk.fileFilterNew
                     Gtk.fileFilterAddPattern xtFilter "*.sav"
                     Gtk.fileFilterSetName xtFilter "PointsXT"
                     Gtk.fileChooserAddFilter fileChooser xtFilter
                     response <- Gtk.dialogRun fileChooser
                     case response of
                       Gtk.ResponseCancel -> Gtk.widgetDestroy fileChooser
                       Gtk.ResponseOk     -> do maybeFileName <- liftM (fmap decodeString) $ Gtk.fileChooserGetFilename fileChooser
                                                Gtk.widgetDestroy fileChooser
                                                case maybeFileName of
                                                  Just fileName -> do tabs <- get tabsRef
                                                                      let gwb = tabs IntMap.! pageNum
                                                                      game <- get (gwbGame gwb)
                                                                      saveResult <- XT.save fileName game
                                                                      unless saveResult $
                                                                        do messageDialog <- Gtk.messageDialogNew (Just (mainWindow window)) [] Gtk.MessageError Gtk.ButtonsOk "Error: not saved."
                                                                           Gtk.dialogRun messageDialog
                                                                           Gtk.widgetDestroy messageDialog
                                                  Nothing       -> return ()
                       _                  -> error "fileChooser: uncknown response."
                return False
          aboutMenuItem window `Gtk.on` Gtk.buttonReleaseEvent $ liftIO $ do
                curAboutDialog <- getAboutDialog
                Gtk.dialogRun curAboutDialog
                Gtk.widgetDestroy curAboutDialog
                return False
          Gtk.widgetShowAll (mainWindow window)
          Gtk.mainGUI
