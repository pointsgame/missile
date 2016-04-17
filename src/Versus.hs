module Versus (main) where

import Control.Monad
import Control.Applicative
import Options.Applicative
import System.Random
import Player
import Field
import Bot

data GenMoveType = Simple | WithTime Int | WithComplexity Int
  deriving (Eq, Show, Read)

data BeginPattern = Empty | Cross
  deriving (Eq, Show, Read)

data Settings = Settings { width :: Int
                         , height :: Int
                         , genMoveType :: GenMoveType
                         , beginPattern :: BeginPattern
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
  fmap WithComplexity (option auto $ long "complexity" <> short 'c' <> metavar "COMPLEXITY" <> help "Use gen_move_with_complexity")

beginPatternParser :: Parser BeginPattern
beginPatternParser = option auto $ long "pattern" <> short 'p' <> metavar "PATTERN" <> help "Begin pattern" <> value Cross

pathParser :: Parser String
pathParser = strArgument $ metavar "BOT"

settingsParser :: Parser Settings
settingsParser = Settings <$> widthParser <*> heightParser <*> genMoveTypeParser <*> beginPatternParser <*> pathParser <*> pathParser

crossMoves :: Int -> Int -> [(Pos, Player)]
crossMoves width' height' = [((width' `div` 2 - 1, height' `div` 2 - 1), Red),
                             ((width' `div` 2, height' `div` 2 - 1), Black),
                             ((width' `div` 2, height' `div` 2), Red),
                             ((width' `div` 2 - 1, height' `div` 2), Black)]

playBotMoves :: Bot -> [(Pos, Player)] -> IO ()
playBotMoves bot moves' = mapM_ (\(pos, player) -> Bot.play bot pos player) moves'

playFieldMoves :: Field -> [(Pos, Player)] -> Field
playFieldMoves field moves' = foldl (\field' (pos, player) -> putPoint pos player field') field moves'

playGame :: RandomGen g => Settings -> Bot -> Bot -> g -> Bool -> IO (Maybe Player)
playGame settings bot1 bot2 rng swap =
  do let (seed1, rng') = next rng
         (seed2, _) = next rng'
     Bot.init bot1 (width settings) (height settings) seed1
     Bot.init bot2 (width settings) (height settings) seed2
     putStrLn $ "Started new game. Swap is " ++ (show swap) ++ "."
     let emptyField' = emptyField (width settings) (height settings)
     field <- case beginPattern settings of
                Empty -> return emptyField'
                Cross -> do let moves' = crossMoves (width settings) (height settings)
                            mapM_ (\(pos, player) -> putStrLn $ "Next move is " ++ (show pos) ++ ", player is " ++ (show player) ++ ".") moves'
                            playBotMoves bot1 moves'
                            playBotMoves bot2 moves'
                            return $ playFieldMoves emptyField' moves'
     playGame' field Red where
       playGame' field player | isFullField field = do putStrLn $ "Game is done! Red score is " ++ (show $ scoreRed field) ++ ". Black score is " ++ (show $ scoreBlack field) ++ "."
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
            putStrLn $ "Red score is " ++ (show $ scoreRed field) ++ ". Black score is " ++ (show $ scoreBlack field) ++ ". Next move is " ++ (show pos) ++ ", player is " ++ (show player) ++ "."
            unless (isPuttingAllowed field pos) $ error "This move is invalid!"
            let nextField = putPoint pos player field --TODO: validate.
            Bot.play bot1 pos player
            Bot.play bot2 pos player
            playGame' nextField $ nextPlayer player

collectStatistics :: RandomGen g => Settings -> Bot -> Bot -> g -> IO ()
collectStatistics settings bot1 bot2 rng = collectStatistics' rng False 0 0 0 where
  collectStatistics' :: RandomGen g => g -> Bool -> Int -> Int -> Int -> IO ()
  collectStatistics' rng' swap wins draws defeats =
    do let (rng1, rng2) = split rng'
       result <- playGame settings bot1 bot2 rng1 swap
       let newWins = if result == Just Red && not swap || result == Just Black && swap then wins + 1 else wins
           newDraws = if result == Nothing then draws + 1 else draws
           newDefeats = if result == Just Black && not swap || result == Just Red && swap then defeats + 1 else defeats
       putStrLn $ "Statistics: " ++ (show newWins) ++ "/" ++ (show newDraws) ++ "/" ++ (show newDefeats)
       collectStatistics' rng2 (not swap) newWins newDraws newDefeats

main :: IO ()
main =
  do settings <- execParser $ info settingsParser (fullDesc <> progDesc "Plays games with one bot against other.")
     bot1 <- Bot.run $ path1 settings
     bot2 <- Bot.run $ path2 settings
     rng <- getStdGen
     collectStatistics settings bot1 bot2 rng
