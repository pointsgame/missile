module Versus (main) where

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
import qualified VersusTable as VT

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
                         , verbose :: Bool
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

verboseParser :: Parser Bool
verboseParser = switch $ long "verbose" <> short 'v' <> help "Verbose output"

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
  verboseParser <*>
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

crossMoves :: Int -> Int -> Player -> [(Pos, Player)]
crossMoves width' height' player =
  [ ((width' `div` 2 - 1, height' `div` 2 - 1), player)
  , ((width' `div` 2, height' `div` 2 - 1), nextPlayer player)
  , ((width' `div` 2, height' `div` 2), player)
  , ((width' `div` 2 - 1, height' `div` 2), nextPlayer player)
  ]

playBotMoves :: Bot -> [(Pos, Player)] -> IO ()
playBotMoves bot = mapM_ $ uncurry (Bot.play bot)

playFieldMoves :: [Field] -> [(Pos, Player)] -> [Field]
playFieldMoves = foldl $ \fields' (pos, player) -> putPoint pos player (head fields') : fields'

playGame :: RandomGen g => Settings -> Bot -> Bot -> g -> Player -> ([Field] -> IO ()) -> IO (Maybe Player)
playGame settings bot1 bot2 rng firstPlayer redraw =
  do let (seed1, rng') = next rng
         (seed2, _) = next rng'
     Bot.init bot1 (width settings) (height settings) seed1
     Bot.init bot2 (width settings) (height settings) seed2
     let emptyField' = emptyField (width settings) (height settings)
     fields <- case beginPattern settings of
                 Empty -> return [emptyField']
                 Cross -> do let moves' = crossMoves (width settings) (height settings) firstPlayer
                             when (verbose settings) $
                               mapM_ (\(pos, player) -> putStrLn $ VT.row player pos 0 0) moves'
                             playBotMoves bot1 moves'
                             playBotMoves bot2 moves'
                             return $ playFieldMoves [emptyField'] moves'
     redraw fields
     playGame' fields firstPlayer where
       playGame' [] _ = error "Internal error."
       playGame' (fields @ (field : _)) player | gameOver field = return $ if | scoreRed field > scoreBlack field -> Just Red
                                                                              | scoreRed field < scoreBlack field -> Just Black
                                                                              | otherwise                         -> Nothing
                                               | otherwise      =
         do let bot = case player of
                        Red   -> bot1
                        Black -> bot2
            pos <- case genMoveType settings of
                     Simple                    -> genMove bot player
                     WithTime time             -> genMoveWithTime bot player time
                     WithComplexity complexity -> genMoveWithComplexity bot player complexity
            unless (isPuttingAllowed field pos) $ error $ "Invalid move: " ++ show pos ++ "!"
            let nextFields = putPoint pos player field : fields
            when (verbose settings) $
              putStrLn $ VT.row player pos (scoreRed $ head nextFields) (scoreBlack $ head nextFields)
            redraw nextFields
            Bot.play bot1 pos player
            Bot.play bot2 pos player
            playGame' nextFields $ nextPlayer player

collectStatistics :: RandomGen g => Settings -> Bot -> Bot -> MVar [Field] -> g -> IO () -> IO ()
collectStatistics settings bot1 bot2 fieldsMVar rng redraw = collectStatistics' rng Red 0 0 0 where
  collectStatistics' :: RandomGen g => g -> Player -> Int -> Int -> Int -> IO ()
  collectStatistics' rng' firstPlayer wins draws defeats =
    do let (rng1, rng2) = split rng'
       when (verbose settings) $ do
         putStrLn $ VT.header1 $ wins + draws + defeats + 1
         putStrLn VT.header2
       result <- playGame settings bot1 bot2 rng1 firstPlayer (\fields -> swapMVar fieldsMVar fields >> redraw)
       when (verbose settings) $
         putStrLn VT.footer
       let (newWins, newDraws, newDefeats) = case result of
             Just Red   -> (wins + 1, draws, defeats)
             Just Black -> (wins, draws, defeats + 1)
             Nothing    -> (wins, draws + 1, defeats)
       putStrLn $ show newWins ++ "/" ++ show newDraws ++ "/" ++ show newDefeats
       if maybe True (> newWins + newDraws + newDefeats) (gamesCount settings)
         then collectStatistics' rng2 (nextPlayer firstPlayer) newWins newDraws newDefeats
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
