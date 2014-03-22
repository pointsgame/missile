module GameWithBot where

import Data.IORef
import Control.Monad
import Data.StateVar
import Data.Maybe
import Settings
import Player
import Field
import Game
import Bot

data GameWithBot = GameWithBot { gwbGame :: IORef Game,
                                 gwbBot :: IORef (Maybe Bot),
                                 gwbBusy :: IORef Bool,
                                 gwbBotError :: IO (),
                                 gwbUpdated :: IO () }

loadBot :: Game -> IO () -> (Bot -> IO ()) -> IO ()
loadBot game callbackError callback =
    let settings = gameSettings game
    in void $ safeRun (aiPath settings) callbackError $ \bot ->
           void $ safeInit bot (gameWidth settings) (gameHeight settings) 0 callbackError $
               if length (gameFields game) == 1
               then callback bot
               else void $ safePlayMany bot (reverse $ moves $ head $ gameFields game) callbackError (callback bot)

loadGWBBot :: GameWithBot -> IO ()
loadGWBBot gwb =
    do game <- get (gwbGame gwb)
       botMaybe <- get (gwbBot gwb)
       if aiPresent (gameSettings game) && isNothing botMaybe
         then do gwbBusy gwb $= True
                 loadBot game (do
                     gwbBusy gwb $= False
                     gwbBotError gwb) $ \bot -> do
                         gwbBot gwb $= Just bot
                         gwbBusy gwb $= False
         else return ()

killGWBBot :: GameWithBot -> IO ()
killGWBBot gwb =
    do botMaybe <- get (gwbBot gwb)
       case botMaybe of
         Nothing  -> return ()
         Just bot -> do safeQuit bot
                        gwbBot gwb $= Nothing

reloadGWBBot :: GameWithBot -> IO ()
reloadGWBBot gwb =
    do killGWBBot gwb
       loadGWBBot gwb

gameWithBot :: Game -> IO () -> IO GameWithBot
gameWithBot game callbackError =
    do busyRef <- newIORef False
       botRef <- newIORef Nothing
       gameRef <- newIORef game
       let gwb = GameWithBot { gwbGame = gameRef,
                               gwbBot = botRef,
                               gwbBusy = busyRef,
                               gwbBotError = callbackError,
                               gwbUpdated = return () }
       loadGWBBot gwb
       return gwb

botError :: GameWithBot -> IO ()
botError gwb =
    do killGWBBot gwb
       modifyIORef (gwbGame gwb) (\game -> game { gameSettings = (gameSettings game) { aiPresent = False } })
       gwbBusy gwb $= False
       gwbBotError gwb

putGWBPlayersPoint' :: Game -> Pos -> Player -> GameWithBot -> IO ()
putGWBPlayersPoint' game pos player gwb =
    do let settings = gameSettings game
           game' = putGamePlayersPoint pos player game
           player' = curPlayer game'
       botMaybe <- get (gwbBot gwb)
       case botMaybe of
         Nothing  -> do gwbGame gwb $= game'
                        gwbUpdated gwb
         Just bot -> do gwbBusy gwb $= True
                        gwbGame gwb $= game'
                        gwbUpdated gwb
                        void $ safePlay bot pos player (botError gwb) $
                                if aiRespondent settings
                                then let gen = case aiGenMoveType settings of
                                                 Simple                    -> safeGenMove bot player'
                                                 WithTime time             -> safeGenMoveWithTime bot player' time
                                                 WithComplexity complexity -> safeGenMoveWithComplexity bot player' complexity
                                     in void $ gen (botError gwb) $ \pos' -> do
                                            let game'' = putGamePlayersPoint pos' player' game'
                                            (gwbGame gwb) $= game''
                                            gwbUpdated gwb
                                            void $ safePlay bot pos' player' (botError gwb) $
                                                gwbBusy gwb $= False
                                else gwbBusy gwb $= False

putGWBPlayersPoint :: Pos -> Player -> GameWithBot -> IO ()
putGWBPlayersPoint pos player gwb =
    do busy <- get (gwbBusy gwb)
       game <- get (gwbGame gwb)
       if busy || (not $ puttingAllow (head $ gameFields $ game) pos)
         then return ()
         else putGWBPlayersPoint' game pos player gwb

putGWBPoint :: Pos -> GameWithBot -> IO ()
putGWBPoint pos gwb =
    do busy <- get (gwbBusy gwb)
       game <- get (gwbGame gwb)
       if busy || (not $ puttingAllow (head $ gameFields $ game) pos)
         then return ()
         else putGWBPlayersPoint' game pos (curPlayer game) gwb

backGWB :: GameWithBot -> IO ()
backGWB gwb =
    do busy <- get (gwbBusy gwb)
       game <- get (gwbGame gwb)
       botMaybe <- get (gwbBot gwb)
       if busy || (length $ gameFields game) == 1
         then return ()
         else case botMaybe of
                Nothing  -> do gwbGame gwb $= backGame game
                               gwbUpdated gwb
                Just bot -> do gwbBusy gwb $= True
                               gwbGame gwb $= backGame game
                               void $ safeUndo bot (botError gwb) $ do
                                       gwbBusy gwb $= False
                                       gwbUpdated gwb

updateGWBSettings :: GameWithBot -> Settings -> IO ()
updateGWBSettings gwb settings =
    do game <- get (gwbGame gwb)
       gwbGame gwb $= updateGameSettings game settings
       let oldSettings = gameSettings game
       if aiPresent oldSettings && not (aiPresent settings)
         then killGWBBot gwb
         else if not (aiPresent oldSettings) && aiPresent settings
         then loadGWBBot gwb
         else if aiPath oldSettings /= aiPath settings
         then reloadGWBBot gwb
         else return ()
       gwbUpdated gwb