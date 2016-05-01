module Versus (main) where

import Data.Maybe
import Data.Array
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
import GameWidget

data GenMoveType = Simple | WithTime Int | WithComplexity Int
  deriving (Eq, Show, Read)

data BeginPattern = Empty | Cross
  deriving (Eq, Show, Read)

data GUIType = Full | Light | None
  deriving (Eq, Show, Read)

data Settings = Settings { width :: Int
                         , height :: Int
                         , genMoveType :: GenMoveType
                         , beginPattern :: BeginPattern
                         , guiType :: GUIType
                         , gamesCount :: Maybe Int
                         , path1 :: String
                         , path2 :: String
                         }

widthParser :: Parser Int
widthParser = option auto $ long "width" <> short 'w' <> metavar "WIDTH" <> help "Field width" <> value 20

heightParser :: Parser Int
heightParser = option auto $ long "height" <> short 'h' <> metavar "HEIGHT" <> help "Field width" <> value 20

genMoveTypeParser :: Parser GenMoveType
genMoveTypeParser =
  flag' Simple (long "simple" <> short 's' <> help "Use gen_move") <|>
  fmap WithTime (option auto $ long "time" <> short 't' <> metavar "TIME" <> help "Use gen_move_with_time") <|>
  fmap WithComplexity (option auto $ long "complexity" <> short 'c' <> metavar "COMPLEXITY" <> help "Use gen_move_with_complexity") <|>
  pure Simple

beginPatternParser :: Parser BeginPattern
beginPatternParser = option auto $ long "pattern" <> short 'p' <> metavar "PATTERN" <> help "Begin pattern" <> value Cross

guiTypeParser :: Parser GUIType
guiTypeParser =
  flag' Light (long "light" <> short 'l' <> help "Draw only game field") <|>
  flag' None (long "nogui" <> short 'n' <> help "Console mode") <|>
  pure Full

gamesCountParser :: Parser (Maybe Int)
gamesCountParser =
  fmap Just (option auto $ long "games" <> short 'g' <> metavar "GAMES" <> help "Games count") <|>
  pure Nothing

pathParser :: Parser String
pathParser = strArgument $ metavar "BOT"

settingsParser :: Parser Settings
settingsParser =
  Settings <$>
  widthParser <*>
  heightParser <*>
  genMoveTypeParser <*>
  beginPatternParser <*>
  guiTypeParser <*>
  gamesCountParser <*>
  pathParser <*>
  pathParser

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

gameOver :: Field -> Bool
gameOver field = notElem EmptyPoint $ elems $ points field

crossMoves :: Int -> Int -> [(Pos, Player)]
crossMoves width' height' = [((width' `div` 2 - 1, height' `div` 2 - 1), Red),
                             ((width' `div` 2, height' `div` 2 - 1), Black),
                             ((width' `div` 2, height' `div` 2), Red),
                             ((width' `div` 2 - 1, height' `div` 2), Black)]

playBotMoves :: Bot -> [(Pos, Player)] -> IO ()
playBotMoves bot = mapM_ $ uncurry (Bot.play bot)

playFieldMoves :: [Field] -> [(Pos, Player)] -> [Field]
playFieldMoves = foldl $ \fields' (pos, player) -> putPoint pos player (head fields') : fields'

playGame :: RandomGen g => Settings -> Bot -> Bot -> g -> Bool -> ([Field] -> IO ()) -> IO (Maybe Player)
playGame settings bot1 bot2 rng swap redraw =
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
     redraw fields
     playGame' fields Red where
       playGame' [] _ = error "Internal error."
       playGame' (fields @ (field : _)) player | gameOver field = do putStrLn $ "Game is done! Red score is " ++ show (scoreRed field) ++ ". Black score is " ++ show (scoreBlack field) ++ "."
                                                                     return $ if | scoreRed field > scoreBlack field -> Just Red
                                                                                 | scoreRed field < scoreBlack field -> Just Black
                                                                                 | otherwise                         -> Nothing
                                               | otherwise      =
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
            redraw fields
            Bot.play bot1 pos player
            Bot.play bot2 pos player
            playGame' nextFields $ nextPlayer player

collectStatistics :: RandomGen g => Settings -> Bot -> Bot -> MVar [Field] -> g -> IO () -> IO ()
collectStatistics settings bot1 bot2 fieldsMVar rng redraw = collectStatistics' rng False 0 0 0 where
  collectStatistics' :: RandomGen g => g -> Bool -> Int -> Int -> Int -> IO ()
  collectStatistics' rng' swap wins draws defeats =
    do let (rng1, rng2) = split rng'
       result <- playGame settings bot1 bot2 rng1 swap (\fields -> swapMVar fieldsMVar fields >> redraw)
       let newWins = if result == Just Red && not swap || result == Just Black && swap then wins + 1 else wins
           newDraws = if isNothing result then draws + 1 else draws
           newDefeats = if result == Just Black && not swap || result == Just Red && swap then defeats + 1 else defeats
       putStrLn $ "Statistics: " ++ show newWins ++ "/" ++ show newDraws ++ "/" ++ show newDefeats
       if maybe True (<= wins + draws + defeats) (gamesCount settings)
         then collectStatistics' rng2 (not swap) newWins newDraws newDefeats
         else do quit bot1
                 quit bot2

forkThread :: IO () -> IO (MVar ())
forkThread p = do
  handle <- newEmptyMVar
  void $ forkFinally p $ const $ putMVar handle ()
  return handle

main :: IO ()
main =
  do settings <- execParser $ info settingsParser (fullDesc <> progDesc "Plays games with one bot against other.")
     bot1 <- Bot.run $ path1 settings
     bot2 <- Bot.run $ path2 settings
     rng <- getStdGen
     fieldsMVar <- newMVar [emptyField (width settings) (height settings)]
     gameWidgetMVar <- newEmptyMVar
     threadMVar <- if guiType settings /= None
       then forkThread $ do
         Gtk.initGUI
         mainWindow <- Gtk.windowNew
         gameWidget <- gameWidgetNew (guiType settings == Light) (readMVar fieldsMVar) (return drawSettings)
         Gtk.windowSetDefaultSize mainWindow 800 600
         mainWindow `Gtk.set` [ Gtk.windowTitle := "Versus"
                              , Gtk.containerChild := toWidget gameWidget
                              ]
         putMVar gameWidgetMVar gameWidget
         mainWindow `Gtk.on` Gtk.deleteEvent $ liftIO $ do
           Gtk.mainQuit
           return False
         Gtk.widgetShowAll mainWindow
         Gtk.mainGUI
         void $ takeMVar gameWidgetMVar
       else newMVar ()
     collectStatistics settings bot1 bot2 fieldsMVar rng $ do
       gameWidgetMaybe <- tryReadMVar gameWidgetMVar
       case gameWidgetMaybe of
         Just gameWidget -> Gtk.postGUIAsync $ Gtk.widgetQueueDraw $ toWidget gameWidget
         Nothing         -> return ()
     isGUIRunning <- isEmptyMVar threadMVar
     when isGUIRunning $ do
       Gtk.postGUIAsync Gtk.mainQuit
       takeMVar threadMVar
