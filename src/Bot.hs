module Bot where

import GHC.IO.Handle
import System.IO
import System.Process
import Control.Monad
import Data.List.Split
import Control.Concurrent
import Control.Exception
import Field

data Bot = Bot { stdInput :: Handle,
                 stdOutput :: Handle,
                 stdError :: Handle,
                 processId :: ProcessHandle }

run :: String -> IO Bot
run path = do (inp, out, err, pid) <- runInteractiveProcess path [] Nothing Nothing
              hSetBuffering inp NoBuffering
              hSetBuffering out NoBuffering
              hSetBuffering err NoBuffering
              return Bot { stdInput = inp,
                           stdOutput = out,
                           stdError = err,
                           processId = pid }

quit :: Bot -> IO ()
quit bot = hPutStrLn (stdInput bot) "0 quit"

listCommands :: Bot -> IO [String]
listCommands bot =
    do hPutStrLn (stdInput bot) "0 list_commands"
       ("=" : "0" : "list_commands" : answer) <- hGetLine (stdOutput bot) >>= return . splitOn " "
       return answer

init :: Bot -> Int -> Int -> Int -> IO ()
init bot width height randomSeed =
    do hPutStrLn (stdInput bot) ("0 init " ++ show width ++ " " ++ show height ++ " " ++ show randomSeed)
       "= 0 init" <- hGetLine (stdOutput bot)
       return ()

name :: Bot -> IO String
name bot =
    do hPutStrLn (stdInput bot) "0 name"
       ("=" : "0" : "name" : [answer]) <- hGetLine (stdOutput bot) >>= return . splitOn " "
       return answer

version :: Bot -> IO String
version bot =
    do hPutStrLn (stdInput bot) "0 version"
       ("=" : "0" : "version" : [answer]) <- hGetLine (stdOutput bot) >>= return . splitOn " "
       return answer

play :: Bot -> Pos -> Player -> IO ()
play bot pos player =
    do let strX = show $ fst pos
           strY = show $ snd pos
           strPlayer = if player == Red then "0" else "1"
       hPutStrLn (stdInput bot) ("0 play " ++ strX ++ " " ++ strY ++ " " ++ strPlayer)
       ("=" : "0" : "play" : answerX : answerY : [answerPlayer]) <- hGetLine (stdOutput bot) >>= return . splitOn " "
       if answerX /= strX || answerY /= strY || answerPlayer /= strPlayer
         then error "play: invalid answer."
         else return ()

genMove :: Bot -> Player -> IO Pos
genMove bot player =
    do let strPlayer = if player == Red then "0" else "1"
       hPutStrLn (stdInput bot) ("0 gen_move " ++ strPlayer)
       ("=" : "0" : "gen_move" : answerX : answerY : [answerPlayer]) <- hGetLine (stdOutput bot) >>= return . splitOn " "
       if answerPlayer /= strPlayer
         then error "genMove: invalid answer."
         else return (read answerX, read answerY)

genMoveWithComplexity :: Bot -> Player -> Int -> IO Pos
genMoveWithComplexity bot player complexity =
    do let strPlayer = if player == Red then "0" else "1"
       hPutStrLn (stdInput bot) ("0 gen_move_with_complexity " ++ strPlayer ++ " " ++ show complexity)
       ("=" : "0" : "gen_move_with_complexity" : answerX : answerY : [answerPlayer]) <- hGetLine (stdOutput bot) >>= return . splitOn " "
       if answerPlayer /= strPlayer
         then error "genMoveWithComplexity: invalid answer."
         else return (read answerX, read answerY)

genMoveWithTime :: Bot -> Player -> Int -> IO Pos
genMoveWithTime bot player time =
    do let strPlayer = if player == Red then "0" else "1"
       hPutStrLn (stdInput bot) ("0 gen_move_with_time " ++ strPlayer ++ " " ++ show time)
       ("=" : "0" : "gen_move_with_time" : answerX : answerY : [answerPlayer]) <- hGetLine (stdOutput bot) >>= return . splitOn " "
       if answerPlayer /= strPlayer
         then error "genMoveWithTime: invalid answer."
         else return (read answerX, read answerY)

undo :: Bot -> IO ()
undo bot =
    do hPutStrLn (stdInput bot) "0 undo"
       "= 0 undo" <- hGetLine (stdOutput bot)
       return ()

safeRun :: String -> IO () -> (Bot -> IO ()) -> IO ThreadId
safeRun path callbackError callback =
    forkIO $ do botEither <- try (run path) :: IO (Either SomeException Bot)
                case botEither of
                  Left _    -> callbackError
                  Right bot -> callback bot

safeQuit :: Bot -> IO ThreadId
safeQuit bot =
    forkIO $ do answerEither <- try (quit bot) :: IO (Either SomeException ())
                case answerEither of
                  Left _  -> terminateProcess (processId bot)
                  Right _ -> do threadDelay 200
                                maybeExitCode <- getProcessExitCode (processId bot)
                                case maybeExitCode of
                                  Nothing -> terminateProcess (processId bot)
                                  Just _  -> return ()
                void $ waitForProcess (processId bot)

safeListCommands :: Bot -> IO () -> ([String] -> IO ()) -> IO ThreadId
safeListCommands bot callbackError callback =
    forkIO $ do answerEither <- try (listCommands bot) :: IO (Either SomeException [String])
                case answerEither of
                  Left _       -> callbackError
                  Right answer -> callback answer

safeInit :: Bot -> Int -> Int -> Int -> IO () -> IO () -> IO ThreadId
safeInit bot width height randomSeed callbackError callback =
    forkIO $ do answerEither <- try (Bot.init bot width height randomSeed) :: IO (Either SomeException ())
                case answerEither of
                  Left _  -> callbackError
                  Right _ -> callback

safeName :: Bot -> IO () -> (String -> IO ()) -> IO ThreadId
safeName bot callbackError callback =
    forkIO $ do answerEither <- try (name bot) :: IO (Either SomeException String)
                case answerEither of
                  Left _       -> callbackError
                  Right answer -> callback answer

safeVarsion :: Bot -> IO () -> (String -> IO ()) -> IO ThreadId
safeVarsion bot callbackError callback =
    forkIO $ do answerEither <- try (version bot) :: IO (Either SomeException String)
                case answerEither of
                  Left _       -> callbackError
                  Right answer -> callback answer

safePlay :: Bot -> Pos -> Player -> IO () -> IO () -> IO ThreadId
safePlay bot pos player callbackError callback =
    forkIO $ do answerEither <- try (play bot pos player) :: IO (Either SomeException ())
                case answerEither of
                  Left _  -> callbackError
                  Right _ -> callback

safeGenMove :: Bot -> Player -> IO () -> (Pos -> IO ()) -> IO ThreadId
safeGenMove bot player callbackError callback =
    forkIO $ do answerEither <- try (genMove bot player) :: IO (Either SomeException Pos)
                case answerEither of
                  Left _       -> callbackError
                  Right answer -> callback answer

safeGenMoveWithComplexity :: Bot -> Player -> Int -> IO () -> (Pos -> IO ()) -> IO ThreadId
safeGenMoveWithComplexity bot player complexity callbackError callback =
    forkIO $ do answerEither <- try (genMoveWithComplexity bot player complexity) :: IO (Either SomeException Pos)
                case answerEither of
                  Left _       -> callbackError
                  Right answer -> callback answer

safeGenMoveWithTime :: Bot -> Player -> Int -> IO () -> (Pos -> IO ()) -> IO ThreadId
safeGenMoveWithTime bot player time callbackError callback =
    forkIO $ do answerEither <- try (genMoveWithTime bot player time) :: IO (Either SomeException Pos)
                case answerEither of
                  Left _       -> callbackError
                  Right answer -> callback answer

safeUndo :: Bot -> IO () -> IO () -> IO ThreadId
safeUndo bot callbackError callback =
    forkIO $ do answerEither <- try (undo bot) :: IO (Either SomeException ())
                case answerEither of
                  Left _  -> callbackError
                  Right _ -> callback

safePlayMany :: Bot -> [(Pos, Player)] -> IO () -> IO () -> IO ThreadId
safePlayMany bot moves' callbackError callback =
    forkIO $ do answerEither <- try (mapM_ (\(pos, player) -> play bot pos player) moves') :: IO (Either SomeException ())
                case answerEither of
                  Left _  -> callbackError
                  Right _ -> callback