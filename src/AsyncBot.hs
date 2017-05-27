module AsyncBot ( asyncRun
                , asyncStop
                , asyncListCommands
                , asyncInit
                , asyncName
                , asyncVersion
                , asyncPlay
                , asyncGenMove
                , asyncGenMoveWithComplexity
                , asyncGenMoveWithTime
                , asyncUndo
                , asyncPlayMany
                ) where

import Control.Concurrent
import Control.Exception
import Player
import Field
import Bot

asyncRun :: String -> IO () -> (Bot -> IO ()) -> IO ThreadId
asyncRun path callbackError callback =
  forkIO $ do
    botEither <- try (run path) :: IO (Either SomeException Bot)
    case botEither of
      Left _    -> callbackError
      Right bot -> callback bot

asyncStop :: Bot -> Int -> IO () -> IO ThreadId
asyncStop bot delay callback =
  forkIO $ do
    stop bot delay
    callback

asyncListCommands :: Bot -> IO () -> ([String] -> IO ()) -> IO ThreadId
asyncListCommands bot callbackError callback =
  forkIO $ do
    answerEither <- try (listCommands bot) :: IO (Either SomeException [String])
    case answerEither of
      Left _       -> callbackError
      Right answer -> callback answer

asyncInit :: Bot -> Int -> Int -> Int -> IO () -> IO () -> IO ThreadId
asyncInit bot width height randomSeed callbackError callback =
  forkIO $ do
    answerEither <- try (Bot.init bot width height randomSeed) :: IO (Either SomeException ())
    case answerEither of
      Left _  -> callbackError
      Right _ -> callback

asyncName :: Bot -> IO () -> (String -> IO ()) -> IO ThreadId
asyncName bot callbackError callback =
  forkIO $ do
    answerEither <- try (name bot) :: IO (Either SomeException String)
    case answerEither of
      Left _       -> callbackError
      Right answer -> callback answer

asyncVersion :: Bot -> IO () -> (String -> IO ()) -> IO ThreadId
asyncVersion bot callbackError callback =
  forkIO $ do
    answerEither <- try (version bot) :: IO (Either SomeException String)
    case answerEither of
      Left _       -> callbackError
      Right answer -> callback answer

asyncPlay :: Bot -> Pos -> Player -> IO () -> IO () -> IO ThreadId
asyncPlay bot pos player callbackError callback =
  forkIO $ do
    answerEither <- try (play bot pos player) :: IO (Either SomeException ())
    case answerEither of
      Left _  -> callbackError
      Right _ -> callback

asyncGenMove :: Bot -> Player -> IO () -> (Pos -> IO ()) -> IO ThreadId
asyncGenMove bot player callbackError callback =
  forkIO $ do
    answerEither <- try (genMove bot player) :: IO (Either SomeException Pos)
    case answerEither of
      Left _       -> callbackError
      Right answer -> callback answer

asyncGenMoveWithComplexity :: Bot -> Player -> Int -> IO () -> (Pos -> IO ()) -> IO ThreadId
asyncGenMoveWithComplexity bot player complexity callbackError callback =
  forkIO $ do
    answerEither <- try (genMoveWithComplexity bot player complexity) :: IO (Either SomeException Pos)
    case answerEither of
      Left _       -> callbackError
      Right answer -> callback answer

asyncGenMoveWithTime :: Bot -> Player -> Int -> IO () -> (Pos -> IO ()) -> IO ThreadId
asyncGenMoveWithTime bot player time callbackError callback =
  forkIO $ do
    answerEither <- try (genMoveWithTime bot player time) :: IO (Either SomeException Pos)
    case answerEither of
      Left _       -> callbackError
      Right answer -> callback answer

asyncUndo :: Bot -> IO () -> IO () -> IO ThreadId
asyncUndo bot callbackError callback =
  forkIO $ do
    answerEither <- try (undo bot) :: IO (Either SomeException ())
    case answerEither of
      Left _  -> callbackError
      Right _ -> callback

asyncPlayMany :: Bot -> [(Pos, Player)] -> IO () -> IO () -> IO ThreadId
asyncPlayMany bot moves' callbackError callback =
  forkIO $ do
    answerEither <- try (mapM_ (uncurry $ play bot) moves') :: IO (Either SomeException ())
    case answerEither of
      Left _  -> callbackError
      Right _ -> callback
