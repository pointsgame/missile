module Game ( Game
            , gameFields
            , gameNew
            , gameInitBots
            , gamePlay
            , gamePutPoint
            , gameStopBots
            ) where

import Data.IORef
import Control.Exception
import Control.Monad
import System.Random
import Async
import Player
import Field
import GameTree
import GameSettings
import Bot

data Game = Game { gGameTree :: IORef GameTree
                 , gGameSettings :: IORef GameSettings
                 , gBusy :: IORef Bool
                 , gPause :: IORef Bool
                 , gRedBot :: IORef (Maybe Bot)
                 , gBlackBot :: IORef (Maybe Bot)
                 , gCallback :: IO ()
                 }

gameGameTree :: Game -> IO GameTree
gameGameTree = readIORef . gGameTree

gameFields :: Game -> IO [Field]
gameFields = fmap gtFields . gameGameTree

gameBotIORef :: Game -> Player -> IORef (Maybe Bot)
gameBotIORef game player =
  case player of
    Red -> gRedBot game
    Black -> gBlackBot game

gameBot :: Game -> Player -> IO (Maybe Bot)
gameBot game = readIORef . gameBotIORef game

genMoveByType :: GenMoveType -> Bot -> Player -> Async Pos
genMoveByType Simple bot player = async $ genMove bot player
genMoveByType (WithTime time) bot player = async $ genMoveWithTime bot player time
genMoveByType (WithComplexity complexity) bot player = async $ genMoveWithComplexity bot player complexity

-- botError :: Game -> Player -> IO ()
-- botError game player = do
--   let botIORef = gameBotIORef game player
--   maybeBot <- readIORef botIORef
--   case maybeBot of
--     Just _ -> do evalContT $ stopBot botIORef 100 >> lift (writeIORef (gBusy game) False)
--                  gError game player
--     Nothing -> return ()

gameBotsPutPoint :: Game -> Pos -> Player -> Async ()
gameBotsPutPoint game pos player = do
  maybeRedBot <- now $ readIORef $ gRedBot game
  maybeBlackBot <- now $ readIORef $ gBlackBot game
  let continue = now $ return ()
      c1 = maybe continue (\bot -> async $ play bot pos player) maybeRedBot
      c2 = maybe continue (\bot -> async $ play bot pos player) maybeBlackBot
  void $ concurrently c1 c2

gamePlayLoop :: Game -> Async ()
gamePlayLoop game =
  callAA $ \exit1 ->
    callAA $ \exit2 ->
      callEE $ \exit3 -> do
        pause <- now $ readIORef $ gPause game
        when pause $ exit1 ()
        gameTree <- now $ readIORef $ gGameTree game
        when (gameTreeIsOver gameTree) $ exit1 ()
        let player = gtCurPlayer gameTree
        maybeBot <- now $ gameBot game player
        pos <- case maybeBot of
          Just bot -> do
            gameSettings <- now $ readIORef $ gGameSettings game
            genMoveByType (gsGenMoveType gameSettings) bot player
          Nothing -> exit2 ()
        unless (isGameTreePuttingAllowed gameTree pos) $ exit3 $ SomeException $ ErrorCall $ "Wrong move" ++ show pos
        let newGameTree = putGameTreePlayersPoint pos player gameTree
        now $ writeIORef (gGameTree game) newGameTree
        now $ gCallback game
        gameBotsPutPoint game pos player
        gamePlayLoop game

playBotMoves :: Bot -> [(Pos, Player)] -> Async ()
playBotMoves bot moves' =
  forM_ moves' $ \(pos, player) -> async $ play bot pos player

initBot :: GameTree -> String -> Async Bot
initBot gameTree path = do --todo callcc and continue in case of failure, kill failed bot
  bot <- async $ run path
  seed <- now randomIO
  async $ Bot.init bot (gameTreeWidth gameTree) (gameTreeHeight gameTree) (abs seed)
  playBotMoves bot (moves $ head $ gtFields gameTree)
  return bot

stopBot :: IORef (Maybe Bot) -> Int -> Async ()
stopBot botIORef delay = do
  maybeBot <- now $ readIORef botIORef
  now $ writeIORef botIORef Nothing
  async $ forM_ maybeBot (`stop` delay)

stopBots :: Game -> Int -> Async ()
stopBots game delay = do
  let c1 = stopBot (gRedBot game) delay
      c2 = stopBot (gBlackBot game) delay
  void $ concurrently c1 c2

initBots :: Game -> Async ()
initBots game = do
  gameSettings <- now $ readIORef $ gGameSettings game
  gameTree <- now $ readIORef $ gGameTree game
  let c1 = forM (gsRedBotPath gameSettings) $ initBot gameTree
      c2 = forM (gsBlackBotPath gameSettings) $ initBot gameTree
  (maybeRedBot, maybeBlackBot) <- concurrently c1 c2
  now $ writeIORef (gRedBot game) maybeRedBot
  now $ writeIORef (gBlackBot game) maybeBlackBot

unlessBusy :: IORef Bool -> Async () -> Async ()
unlessBusy busyIORef c = do
  busy <- now $ readIORef busyIORef
  unless busy c

withBusy :: IORef Bool -> Async () -> Async ()
withBusy busyIORef c = do
  now $ writeIORef busyIORef True
  c
  now $ writeIORef busyIORef False

unlessWithBusy :: IORef Bool -> Async () -> Async ()
unlessWithBusy busyIORef = unlessBusy busyIORef . withBusy busyIORef

crossMoves :: Int -> Int -> Player -> [(Pos, Player)]
crossMoves width height player =
  [ ((width `div` 2 - 1, height `div` 2 - 1), player)
  , ((width `div` 2, height `div` 2 - 1), nextPlayer player)
  , ((width `div` 2, height `div` 2), player)
  , ((width `div` 2 - 1, height `div` 2), nextPlayer player)
  ]

playFieldMoves :: [Field] -> [(Pos, Player)] -> [Field]
playFieldMoves = foldl $ \fields (pos, player) -> putPoint pos player (head fields) : fields

beginPatternFields :: BeginPattern -> Player -> Int -> Int -> [Field]
beginPatternFields Empty _ width height = [emptyField width height]
beginPatternFields Cross player width height = playFieldMoves [emptyField width height] $ crossMoves width height player

beginPatternGameTree :: BeginPattern -> Int -> Int -> GameTree
beginPatternGameTree beginPattern width height =
  buildGameTree $ beginPatternFields beginPattern Red width height

gameLoad :: GameTree -> GameSettings -> IO () -> IO Game
gameLoad gameTree gameSettings callback = do
  gameTreeIORef <- newIORef gameTree
  gameSettingsIORef <- newIORef gameSettings
  busyIORef <- newIORef False
  pauseIORef <- newIORef False
  redBotIORef <- newIORef Nothing
  blackBotIORef <- newIORef Nothing
  return Game { gGameTree = gameTreeIORef
              , gGameSettings = gameSettingsIORef
              , gBusy = busyIORef
              , gPause = pauseIORef
              , gRedBot = redBotIORef
              , gBlackBot = blackBotIORef
              , gCallback = callback
              }

gameNew :: GameSettings -> IO () -> IO Game
gameNew gameSettings =
  let gameTree = beginPatternGameTree (gsBeginPattern gameSettings) (gsWidth gameSettings) (gsHeight gameSettings)
  in gameLoad gameTree gameSettings

gameInitBots :: Game -> Async ()
gameInitBots game =
  unlessWithBusy (gBusy game) $ do
    initBots game
    gamePlayLoop game

gamePlay :: Game -> Async ()
gamePlay game =
  unlessWithBusy (gBusy game) $ gamePlayLoop game

gamePutPoint :: Game -> Pos -> Async ()
gamePutPoint game pos =
  callAA $ \exit -> do
    gameTree <- now $ readIORef $ gGameTree game
    unless (isGameTreePuttingAllowed gameTree pos) $ exit ()
    busy <- now $ readIORef $ gBusy game
    when busy $ exit ()
    now $ writeIORef (gBusy game) True
    let player = gtCurPlayer gameTree
        newGameTree = putGameTreePlayersPoint pos player gameTree
    now $ writeIORef (gGameTree game) newGameTree
    now $ gCallback game
    gameBotsPutPoint game pos player
    gamePlayLoop game
    now $ writeIORef (gBusy game) False

gameStopBots :: Game -> Int -> Async ()
gameStopBots game delay =
  withBusy (gBusy game) $ stopBots game delay
