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
import Control.Monad.Trans.Except
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

data BotException e = BotExceptionRed e | BotExceptionBlack e

instance Show e => Show (BotException e) where
  show (BotExceptionRed exception) = "Red bot error: " ++ show exception
  show (BotExceptionBlack exception) = "Black bot error: " ++ show exception

instance Exception e => Exception (BotException e)

withBotError :: Player -> Async a -> Async a
withBotError Red = flip catchE $ err . SomeException . BotExceptionRed
withBotError Black = flip catchE $ err . SomeException . BotExceptionBlack

asyncBot :: Player -> IO a -> Async a
asyncBot player = withBotError player . async

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
genMoveByType Simple bot player = asyncBot player $ genMove bot player
genMoveByType (WithTime time) bot player = asyncBot player $ genMoveWithTime bot player time
genMoveByType (WithComplexity complexity) bot player = asyncBot player $ genMoveWithComplexity bot player complexity

gameBotsPutPoint :: Game -> Pos -> Player -> Async ()
gameBotsPutPoint game pos player = do
  maybeRedBot <- now $ readIORef $ gRedBot game
  maybeBlackBot <- now $ readIORef $ gBlackBot game
  let continue = now $ return ()
      c1 = maybe continue (\bot -> asyncBot Red $ play bot pos player) maybeRedBot
      c2 = maybe continue (\bot -> asyncBot Black $ play bot pos player) maybeBlackBot
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
initBot gameTree path = do
  bot <- async $ run path
  seed <- now randomIO
  let f e' = async (stop bot 100) >> err e'
  flip catchE f $ async $ Bot.init bot (gameTreeWidth gameTree) (gameTreeHeight gameTree) (abs seed)
  flip catchE f $ playBotMoves bot (moves $ head $ gtFields gameTree)
  return bot

stopBot :: IORef (Maybe Bot) -> Int -> Async ()
stopBot botIORef delay = do
  maybeBot <- now $ readIORef botIORef
  now $ writeIORef botIORef Nothing
  async $ forM_ maybeBot (`stop` delay)

stopBots :: Game -> Int -> Async ()
stopBots game delay = do
  let c1 = withBotError Red $ stopBot (gRedBot game) delay
      c2 = withBotError Black $ stopBot (gBlackBot game) delay
  void $ concurrently c1 c2

initBots :: Game -> Async ()
initBots game = do
  gameSettings <- now $ readIORef $ gGameSettings game
  gameTree <- now $ readIORef $ gGameTree game
  let c1 = do maybeRedBot <- forM (gsRedBotPath gameSettings) $ withBotError Red . initBot gameTree
              now $ writeIORef (gRedBot game) maybeRedBot
      c2 = do maybeBlackBot <- forM (gsBlackBotPath gameSettings) $ withBotError Black . initBot gameTree
              now $ writeIORef (gBlackBot game) maybeBlackBot
  void $ concurrently c1 c2

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

crossMoves :: Player -> Int -> Int -> [(Pos, Player)]
crossMoves player width height =
  [ ((width `div` 2 - 1, height `div` 2 - 1), player)
  , ((width `div` 2 - 1, height `div` 2), nextPlayer player)
  , ((width `div` 2, height `div` 2), player)
  , ((width `div` 2, height `div` 2 - 1), nextPlayer player)
  ]

twoCrossesMoves :: Player -> Int -> Int -> [(Pos, Player)]
twoCrossesMoves player width height =
  [ ((width `div` 2 - 2, height `div` 2 - 1), player)
  , ((width `div` 2 - 2, height `div` 2), nextPlayer player)
  , ((width `div` 2 - 1, height `div` 2), player)
  , ((width `div` 2 - 1, height `div` 2 - 1), nextPlayer player)
  , ((width `div` 2, height `div` 2), player)
  , ((width `div` 2, height `div` 2 - 1), nextPlayer player)
  , ((width `div` 2 + 1, height `div` 2 - 1), player)
  , ((width `div` 2 + 1, height `div` 2), nextPlayer player)
  ]

tripleCrossMoves :: Player -> Int -> Int -> [(Pos, Player)]
tripleCrossMoves player width height =
  [ ((width `div` 2 - 1, height `div` 2 - 1), player)
  , ((width `div` 2 - 1, height `div` 2), nextPlayer player)
  , ((width `div` 2, height `div` 2), player)
  , ((width `div` 2, height `div` 2 - 1), nextPlayer player)
  , ((width `div` 2 + 1, height `div` 2 - 1), player)
  , ((width `div` 2, height `div` 2 - 2), nextPlayer player)
  , ((width `div` 2, height `div` 2 + 1), player)
  , ((width `div` 2 + 1, height `div` 2), nextPlayer player)
  ]

playFieldMoves :: [Field] -> [(Pos, Player)] -> [Field]
playFieldMoves = foldl $ \fields (pos, player) -> putPoint pos player (head fields) : fields

beginPatternMoves :: BeginPattern -> Player -> Int -> Int -> [(Pos, Player)]
beginPatternMoves Empty = const $ const $ const []
beginPatternMoves Cross = crossMoves
beginPatternMoves TwoCrosses = twoCrossesMoves
beginPatternMoves TripleCross = tripleCrossMoves

beginPatternFields :: BeginPattern -> Player -> Int -> Int -> [Field]
beginPatternFields beginPattern player width height =
  playFieldMoves [emptyField width height] $ beginPatternMoves beginPattern player width height

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
