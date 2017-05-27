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
import Bot
import ContBot

data Game = Game { gGameTree :: IORef GameTree
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
        Just bot -> contGenMove bot player $ (gError game) player
        Nothing -> exit2 ()
      unless (isGameTreePuttingAllowed gameTree pos) $ (lift $ (gError game) player) >> exit1 ()
      let newGameTree = putGameTreePlayersPoint pos player gameTree
      lift $ writeIORef (gGameTree game) newGameTree
      lift $ gCallback game
      gameBotsPutPoint game pos player
      gamePlayLoop game

initBot :: GameTree -> String -> IO () -> ContT () IO Bot
initBot gameTree path callbackError = do --todo callcc and continue in case of failure, kill failed bot, play start position
  bot <- contRun path callbackError
  contInit bot (gameTreeWidth gameTree) (gameTreeHeight gameTree) 0 callbackError
  return bot

gameNew :: GameTree -> (Player -> IO ()) -> IO () -> IO Game
gameNew gameTree callbackError callback = do
  gameTreeIORef <- newIORef gameTree
  busyIORef <- newIORef False
  pauseIORef <- newIORef False
  redBotIORef <- newIORef Nothing
  blackBotIORef <- newIORef Nothing
  return Game { gGameTree = gameTreeIORef
              , gBusy = busyIORef
              , gPause = pauseIORef
              , gRedBot = redBotIORef
              , gBlackBot = blackBotIORef
              , gCallback = callback
              , gError = callbackError
              }

gameInitBots :: Game -> Maybe String -> Maybe String -> IO ()
gameInitBots game maybeRedBotPath maybeBlackBotPath =
  evalContT $ callCC $ \exit -> do
    busy <- lift $ readIORef $ gBusy game
    when busy $ exit ()
    lift $ writeIORef (gBusy game) True
    gameTree <- lift $ readIORef $ gGameTree game
    maybeRedBot <- forM maybeRedBotPath $ flip (initBot gameTree) $ (gError game) Red
    maybeBlackBot <- forM maybeBlackBotPath $ flip (initBot gameTree) $ (gError game) Black
    lift $ writeIORef (gRedBot game) maybeRedBot
    lift $ writeIORef (gBlackBot game) maybeBlackBot
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
    lift $ writeIORef (gBusy game) True
    maybeRedBot <- lift $ readIORef $ gRedBot game
    maybeBlackBot <- lift $ readIORef $ gBlackBot game
    forM_ maybeRedBot (flip contStop delay) *> forM_ maybeBlackBot (flip contStop delay)
    lift $ writeIORef (gRedBot game) Nothing
    lift $ writeIORef (gBlackBot game) Nothing
    lift $ writeIORef (gBusy game) False
    lift $ callback
