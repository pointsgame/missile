module Versus (main) where

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent
import System.Random
import Options.Applicative
import Data.Colour.RGBSpace
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import Player
import Field
import Bot
import Rendering

data GenMoveType = Simple | WithTime Int | WithComplexity Int
  deriving (Eq, Show, Read)

data BeginPattern = Empty | Cross
  deriving (Eq, Show, Read)

data Settings = Settings { width :: Int
                         , height :: Int
                         , genMoveType :: GenMoveType
                         , beginPattern :: BeginPattern
                         , noGUI :: Bool
                         , path1 :: String
                         , path2 :: String
                         }

data MainWindow = MainWindow { window :: Gtk.Window,
                               drawingArea :: Gtk.DrawingArea
                             }

widthParser :: Parser Int
widthParser = option auto $ long "width" <> short 'w' <> metavar "WIDTH" <> help "Field width" <> value 20

heightParser :: Parser Int
heightParser = option auto $ long "height" <> short 'h' <> metavar "HEIGHT" <> help "Field width" <> value 20

genMoveTypeParser :: Parser GenMoveType
genMoveTypeParser =
  flag' Simple (long "simple" <> short 's' <> help "Use gen_move") <|>
  fmap WithTime (option auto $ long "time" <> short 't' <> metavar "TIME" <> help "Use gen_move_with_time") <|>
  fmap WithComplexity (option auto $ long "complexity" <> short 'c' <> metavar "COMPLEXITY" <> help "Use gen_move_with_complexity")

beginPatternParser :: Parser BeginPattern
beginPatternParser = option auto $ long "pattern" <> short 'p' <> metavar "PATTERN" <> help "Begin pattern" <> value Cross

noGUIParser :: Parser Bool
noGUIParser = switch $ long "nogui" <> short 'n' <> help "Console mode"

pathParser :: Parser String
pathParser = strArgument $ metavar "BOT"

settingsParser :: Parser Settings
settingsParser = Settings <$> widthParser <*> heightParser <*> genMoveTypeParser <*> beginPatternParser <*> noGUIParser <*> pathParser <*> pathParser

drawSettings :: DrawSettings
drawSettings = DrawSettings { dsHReflection = False
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

crossMoves :: Int -> Int -> [(Pos, Player)]
crossMoves width' height' = [((width' `div` 2 - 1, height' `div` 2 - 1), Red),
                             ((width' `div` 2, height' `div` 2 - 1), Black),
                             ((width' `div` 2, height' `div` 2), Red),
                             ((width' `div` 2 - 1, height' `div` 2), Black)]

playBotMoves :: Bot -> [(Pos, Player)] -> IO ()
playBotMoves bot = mapM_ $ uncurry (Bot.play bot)

playFieldMoves :: [Field] -> [(Pos, Player)] -> [Field]
playFieldMoves = foldl $ \fields' (pos, player) -> putPoint pos player (head fields') : fields'

playGame :: RandomGen g => Settings -> Bot -> Bot -> MVar [Field] -> g -> Bool -> IO () -> IO (Maybe Player)
playGame settings bot1 bot2 fieldsMVar rng swap redraw =
  do let (seed1, rng') = next rng
         (seed2, _) = next rng'
     Bot.init bot1 (width settings) (height settings) seed1
     Bot.init bot2 (width settings) (height settings) seed2
     putStrLn $ "Started new game. Swap is " ++ show swap ++ "."
     let emptyField' = emptyField (width settings) (height settings)
     fields <- case beginPattern settings of
                 Empty -> return [emptyField']
                 Cross -> do let moves' = crossMoves (width settings) (height settings)
                             mapM_ (\(pos, player) -> putStrLn $ "Next move is " ++ show pos ++ ", player is " ++ show player ++ ".") moves'
                             playBotMoves bot1 moves'
                             playBotMoves bot2 moves'
                             return $ playFieldMoves [emptyField'] moves'
     void $ swapMVar fieldsMVar fields
     redraw
     playGame' fields Red where
       playGame' [] _ = error "Internal error."
       playGame' (fields @ (field : _)) player | isFullField field = do putStrLn $ "Game is done! Red score is " ++ show (scoreRed field) ++ ". Black score is " ++ show (scoreBlack field) ++ "."
                                                                        return $ if | scoreRed field > scoreBlack field -> Just Red
                                                                                    | scoreRed field < scoreBlack field -> Just Black
                                                                                    | otherwise                         -> Nothing
                                               | otherwise         =
         do let bot = case player of
                        Red   -> if swap then bot2 else bot1
                        Black -> if swap then bot1 else bot2
            pos <- case genMoveType settings of
                     Simple                    -> genMove bot player
                     WithTime time             -> genMoveWithTime bot player time
                     WithComplexity complexity -> genMoveWithComplexity bot player complexity
            putStrLn $ "Red score is " ++ show (scoreRed field) ++ ". Black score is " ++ show (scoreBlack field) ++ ". Next move is " ++ show pos ++ ", player is " ++ show player ++ "."
            unless (isPuttingAllowed field pos) $ error "This move is invalid!"
            let nextFields = putPoint pos player field : fields
            void $ swapMVar fieldsMVar nextFields
            redraw
            Bot.play bot1 pos player
            Bot.play bot2 pos player
            playGame' nextFields $ nextPlayer player

collectStatistics :: RandomGen g => Settings -> Bot -> Bot -> MVar [Field] -> g -> IO () -> IO ()
collectStatistics settings bot1 bot2 fieldsMVar rng redraw = collectStatistics' rng False 0 0 0 where
  collectStatistics' :: RandomGen g => g -> Bool -> Int -> Int -> Int -> IO ()
  collectStatistics' rng' swap wins draws defeats =
    do let (rng1, rng2) = split rng'
       result <- playGame settings bot1 bot2 fieldsMVar rng1 swap redraw
       let newWins = if result == Just Red && not swap || result == Just Black && swap then wins + 1 else wins
           newDraws = if isNothing result then draws + 1 else draws
           newDefeats = if result == Just Black && not swap || result == Just Red && swap then defeats + 1 else defeats
       putStrLn $ "Statistics: " ++ show newWins ++ "/" ++ show newDraws ++ "/" ++ show newDefeats
       collectStatistics' rng2 (not swap) newWins newDraws newDefeats

mainWindowNew :: IO MainWindow
mainWindowNew = do
  window' <- Gtk.windowNew
  drawingArea' <- Gtk.drawingAreaNew
  Gtk.windowSetDefaultSize window' 800 600
  window' `Gtk.set` [ Gtk.windowTitle := "Versus",
                      Gtk.containerChild := drawingArea' ]
  return MainWindow { window = window',
                      drawingArea = drawingArea'
                    }

main :: IO ()
main =
  do settings <- execParser $ info settingsParser (fullDesc <> progDesc "Plays games with one bot against other.")
     bot1 <- Bot.run $ path1 settings
     bot2 <- Bot.run $ path2 settings
     rng <- getStdGen
     fieldsMVar <- newMVar [emptyField (width settings) (height settings)]
     mainWindowMVar <- newEmptyMVar
     unless (noGUI settings) $ void $ forkIO $ do
       Gtk.initGUI
       mainWindow <- mainWindowNew
       putMVar mainWindowMVar mainWindow
       window mainWindow `Gtk.on` Gtk.deleteEvent $ liftIO $ do
         void $ takeMVar mainWindowMVar
         Gtk.mainQuit
         return False
       drawingArea mainWindow `Gtk.on` Gtk.draw $ do
         fields <- liftIO $ readMVar fieldsMVar
         width' <- liftIO $ Gtk.widgetGetAllocatedWidth $ drawingArea mainWindow
         height' <- liftIO $ Gtk.widgetGetAllocatedHeight $ drawingArea mainWindow
         draw drawSettings (fromIntegral width') (fromIntegral height') fields
       Gtk.widgetShowAll $ window mainWindow
       Gtk.mainGUI
     collectStatistics settings bot1 bot2 fieldsMVar rng $ do
       mainWindowMaybe <- tryReadMVar mainWindowMVar
       case mainWindowMaybe of
         Just mainWindow -> Gtk.postGUIAsync $ Gtk.widgetQueueDraw $ drawingArea mainWindow
         Nothing         -> return ()
