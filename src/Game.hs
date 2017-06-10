module Game ( Game
            , gameFields
            , gameNew
            , gameInitBots
            , gamePlay
            , gamePutPoint
            , gameStopBots
            ) where

import Data.IORef
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
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

gameGameTree :: Game -> IO GameTree
gameGameTree = readIORef . gGameTree

gameFields :: Game -> IO [Field]
gameFields = fmap gtFields . gameGameTree

gameBot :: Game -> Player -> IO (Maybe Bot)
gameBot game player =
  readIORef $ case player of
    Red -> gRedBot game
    Black -> gBlackBot game

genMoveByType :: GenMoveType -> Bot -> Player -> IO () -> ContT () IO Pos
genMoveByType Simple bot player = contGenMove bot player
genMoveByType (WithTime time) bot player = contGenMoveWithTime bot player time
genMoveByType (WithComplexity complexity) bot player = contGenMoveWithComplexity bot player complexity

gameBotsPutPoint :: Game -> Pos -> Player -> ContT () IO ()
gameBotsPutPoint game pos player = do
  let continue = lift $ return ()
  maybeRedBot <- lift $ readIORef $ gRedBot game
  maybe continue (\bot -> contPlay bot pos player $ (gError game) Red) maybeRedBot
  maybeBlackBot <- lift $ readIORef $ gBlackBot game
  maybe continue (\bot -> contPlay bot pos player $ (gError game) Black) maybeBlackBot

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
          genMoveByType (gsGenMoveType gameSettings) bot player $ (gError game) player
        Nothing -> exit2 ()
      unless (isGameTreePuttingAllowed gameTree pos) $ (lift $ (gError game) player) >> exit1 ()
      let newGameTree = putGameTreePlayersPoint pos player gameTree
      lift $ writeIORef (gGameTree game) newGameTree
      lift $ gCallback game
      gameBotsPutPoint game pos player
      gamePlayLoop game

playBotMoves :: Bot -> [(Pos, Player)] -> IO () -> ContT () IO ()
playBotMoves bot moves callbackError =
  forM_ moves $ \(pos, player) -> contPlay bot pos player callbackError

initBot :: GameTree -> String -> IO () -> ContT () IO Bot
initBot gameTree path callbackError = do --todo callcc and continue in case of failure, kill failed bot
  bot <- contRun path callbackError
  contInit bot (gameTreeWidth gameTree) (gameTreeHeight gameTree) 0 callbackError
  playBotMoves bot (moves $ head $ gtFields gameTree) callbackError
  return bot

stopBots :: Game -> Int -> ContT () IO ()
stopBots game delay = do
  maybeRedBot <- lift $ readIORef $ gRedBot game
  maybeBlackBot <- lift $ readIORef $ gBlackBot game
  forM_ maybeRedBot (flip contStop delay) *> forM_ maybeBlackBot (flip contStop delay)
  lift $ writeIORef (gRedBot game) Nothing
  lift $ writeIORef (gBlackBot game) Nothing

initBots :: Game -> ContT () IO ()
initBots game = do
  gameSettings <- lift $ readIORef $ gGameSettings game
  gameTree <- lift $ readIORef $ gGameTree game
  maybeRedBot <- forM (gsRedBotPath gameSettings) $ flip (initBot gameTree) $ (gError game) Red
  maybeBlackBot <- forM (gsBlackBotPath gameSettings) $ flip (initBot gameTree) $ (gError game) Black
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
  evalContT $ callCC $ \exit -> do
    busy <- lift $ readIORef $ gBusy game
    when busy $ exit ()
    lift $ writeIORef (gBusy game) True
    initBots game
    gamePlayLoop game
    lift $ writeIORef (gBusy game) False

gamePlay :: Game -> IO ()
gamePlay game =
  evalContT $ callCC $ \exit -> do
    busy <- lift $ readIORef $ gBusy game
    when busy $ exit ()
    lift $ writeIORef (gBusy game) True
    gamePlayLoop game
    lift $ writeIORef (gBusy game) False

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
