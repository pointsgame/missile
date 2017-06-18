module Game ( Game
            , gameFields
            , gameNew
            , gameInitBots
            , gamePlay
            , gamePutPoint
            , gameStopBots
            ) where

import Data.IORef
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import System.Random
import Player
import Field
import GameTree
import GameSettings
import Bot
import ContBot

data Game = Game { gGameTree :: IORef GameTree
                 , gGameSettings :: IORef GameSettings
                 , gBusy :: IORef Bool
                 , gPause :: IORef Bool
                 , gRedBot :: IORef (Maybe Bot)
                 , gBlackBot :: IORef (Maybe Bot)
                 , gCallback :: IO ()
                 , gError :: Player -> IO ()
                 }

parallelC :: ContT () IO a -> ContT () IO b -> ContT () IO (a, b)
parallelC c1 c2 = do
  aMVar <- lift newEmptyMVar
  bMVar <- lift newEmptyMVar
  _ <- lift $ forkIO $ runContT c1 $ putMVar aMVar
  _ <- lift $ forkIO $ runContT c2 $ putMVar bMVar
  a <- lift $ takeMVar aMVar
  b <- lift $ takeMVar bMVar
  return (a, b)

asyncC :: ContT () IO a -> ContT () IO a
asyncC (ContT f) =
  ContT $ void . forkIO . f

parallelAsyncC :: ContT () IO a -> ContT () IO b -> ContT () IO (a, b)
parallelAsyncC c1 c2 = asyncC $ parallelC c1 c2

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

genMoveByType :: GenMoveType -> Bot -> Player -> IO () -> ContT () IO Pos
genMoveByType Simple bot player = contGenMove bot player
genMoveByType (WithTime time) bot player = contGenMoveWithTime bot player time
genMoveByType (WithComplexity complexity) bot player = contGenMoveWithComplexity bot player complexity

botError :: Game -> Player -> IO ()
botError game player = do
  let botIORef = gameBotIORef game player
  maybeBot <- readIORef botIORef
  case maybeBot of
    Just _ -> do evalContT $ stopBot botIORef 100 >> lift (writeIORef (gBusy game) False)
                 gError game player
    Nothing -> return ()

gameBotsPutPoint :: Game -> Pos -> Player -> ContT () IO ()
gameBotsPutPoint game pos player = do
  maybeRedBot <- lift $ readIORef $ gRedBot game
  maybeBlackBot <- lift $ readIORef $ gBlackBot game
  let continue = lift $ return ()
      c1 = maybe continue (\bot -> contPlay bot pos player $ botError game Red) maybeRedBot
      c2 = maybe continue (\bot -> contPlay bot pos player $ botError game Black) maybeBlackBot
  void $ parallelAsyncC c1 c2

gamePlayLoop :: Game -> ContT () IO ()
gamePlayLoop game =
  callCC $ \exit1 ->
    callCC $ \exit2 -> do
      pause <- lift $ readIORef $ gPause game
      when pause $ exit1 ()
      gameTree <- lift $ readIORef $ gGameTree game
      when (gameTreeIsOver gameTree) $ exit1 ()
      let player = gtCurPlayer gameTree
      maybeBot <- lift $ gameBot game player
      pos <- case maybeBot of
        Just bot -> do
          gameSettings <- lift $ readIORef $ gGameSettings game
          genMoveByType (gsGenMoveType gameSettings) bot player $ botError game player
        Nothing -> exit2 ()
      unless (isGameTreePuttingAllowed gameTree pos) $ (lift $ botError game player) >> exit1 ()
      let newGameTree = putGameTreePlayersPoint pos player gameTree
      lift $ writeIORef (gGameTree game) newGameTree
      lift $ gCallback game
      gameBotsPutPoint game pos player
      gamePlayLoop game

playBotMoves :: Bot -> [(Pos, Player)] -> IO () -> ContT () IO ()
playBotMoves bot moves' callbackError =
  forM_ moves' $ \(pos, player) -> contPlay bot pos player callbackError

initBot :: GameTree -> String -> IO () -> ContT () IO Bot
initBot gameTree path callbackError = do --todo callcc and continue in case of failure, kill failed bot
  bot <- contRun path callbackError
  seed <- lift randomIO
  contInit bot (gameTreeWidth gameTree) (gameTreeHeight gameTree) seed callbackError
  playBotMoves bot (moves $ head $ gtFields gameTree) callbackError
  return bot

stopBot :: IORef (Maybe Bot) -> Int -> ContT () IO ()
stopBot botIORef delay = do
  maybeBot <- lift $ readIORef botIORef
  lift $ writeIORef botIORef Nothing
  forM_ maybeBot (flip contStop delay)

stopBots :: Game -> Int -> ContT () IO ()
stopBots game delay = do
  let c1 = stopBot (gRedBot game) delay
      c2 = stopBot (gBlackBot game) delay
  void $ parallelAsyncC c1 c2

initBots :: Game -> ContT () IO ()
initBots game = do
  gameSettings <- lift $ readIORef $ gGameSettings game
  gameTree <- lift $ readIORef $ gGameTree game
  let c1 = forM (gsRedBotPath gameSettings) $ flip (initBot gameTree) $ botError game Red
      c2 = forM (gsBlackBotPath gameSettings) $ flip (initBot gameTree) $ botError game Black
  (maybeRedBot, maybeBlackBot) <- parallelAsyncC c1 c2
  lift $ writeIORef (gRedBot game) maybeRedBot
  lift $ writeIORef (gBlackBot game) maybeBlackBot

unlessBusy :: IORef Bool -> ContT () IO () -> ContT () IO ()
unlessBusy busyIORef c = do
  busy <- lift $ readIORef busyIORef
  unless busy c

withBusy :: IORef Bool -> ContT () IO () -> ContT () IO ()
withBusy busyIORef c = do
  lift $ writeIORef busyIORef True
  c
  lift $ writeIORef busyIORef False

unlessWithBusy :: IORef Bool -> ContT () IO () -> ContT () IO ()
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

gameLoad :: GameTree -> GameSettings -> (Player -> IO ()) -> IO () -> IO Game
gameLoad gameTree gameSettings callbackError callback = do
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
              , gError = callbackError
              }

gameNew :: GameSettings -> (Player -> IO ()) -> IO () -> IO Game
gameNew gameSettings =
  let gameTree = beginPatternGameTree (gsBeginPattern gameSettings) (gsWidth gameSettings) (gsHeight gameSettings)
  in gameLoad gameTree gameSettings

gameInitBots :: Game -> IO ()
gameInitBots game =
  evalContT $ unlessWithBusy (gBusy game) $ do
    initBots game
    gamePlayLoop game

gamePlay :: Game -> IO ()
gamePlay game =
  evalContT $ unlessWithBusy (gBusy game) $ gamePlayLoop game

gamePutPoint :: Game -> Pos -> IO ()
gamePutPoint game pos =
  evalContT $ callCC $ \exit -> do
    gameTree <- lift $ readIORef $ gGameTree game
    unless (isGameTreePuttingAllowed gameTree pos) $ exit ()
    busy <- lift $ readIORef $ gBusy game
    when busy $ exit ()
    lift $ writeIORef (gBusy game) True
    let player = gtCurPlayer gameTree
        newGameTree = putGameTreePlayersPoint pos player gameTree
    lift $ writeIORef (gGameTree game) newGameTree
    lift $ gCallback game
    gameBotsPutPoint game pos player
    gamePlayLoop game
    lift $ writeIORef (gBusy game) False

gameStopBots :: Game -> Int -> IO () -> IO ()
gameStopBots game delay callback =
  evalContT $ do
    withBusy (gBusy game) $ stopBots game delay
    lift $ callback
