module ContBot ( contRun
               , contStop
               , contListCommands
               , contInit
               , contName
               , contVersion
               , contPlay
               , contGenMove
               , contGenMoveWithComplexity
               , contGenMoveWithTime
               , contUndo
               , contPlayMany
               ) where

import Control.Monad
import Control.Monad.Trans.Cont
import Player
import Field
import Bot
import AsyncBot

cb :: (a -> b) -> (() -> a) -> b
cb f g = (f . g) ()

contRun :: String -> IO () -> ContT () IO Bot
contRun path =
  ContT . fmap void . asyncRun path

contStop :: Bot -> Int -> ContT () IO ()
contStop bot =
  ContT . fmap void . cb . asyncStop bot

contListCommands :: Bot -> IO () -> ContT () IO [String]
contListCommands bot =
  ContT . fmap void . asyncListCommands bot

contInit :: Bot -> Int -> Int -> Int -> IO () -> ContT () IO ()
contInit bot width height randomSeed =
  ContT . fmap void . cb . asyncInit bot width height randomSeed

contName :: Bot -> IO () -> ContT () IO String
contName bot =
  ContT . fmap void . asyncName bot

contVersion :: Bot -> IO () -> ContT () IO String
contVersion bot =
  ContT . fmap void . asyncVersion bot

contPlay :: Bot -> Pos -> Player -> IO () -> ContT () IO ()
contPlay bot pos player =
  ContT . fmap void . cb . asyncPlay bot pos player

contGenMove :: Bot -> Player -> IO () -> ContT () IO Pos
contGenMove bot player =
  ContT . fmap void . asyncGenMove bot player

contGenMoveWithComplexity :: Bot -> Player -> Int -> IO () -> ContT () IO Pos
contGenMoveWithComplexity bot player complexity =
  ContT . fmap void . asyncGenMoveWithComplexity bot player complexity

contGenMoveWithTime :: Bot -> Player -> Int -> IO () -> ContT () IO Pos
contGenMoveWithTime bot player time =
  ContT . fmap void . asyncGenMoveWithTime bot player time

contUndo :: Bot -> IO () -> ContT () IO ()
contUndo bot =
  ContT . fmap void . cb . asyncUndo bot

contPlayMany :: Bot -> [(Pos, Player)] -> IO () -> ContT () IO ()
contPlayMany bot moves' =
  ContT . fmap void . cb . asyncPlayMany bot moves'
