module Async ( Async
             , now
             , async
             , err
             , callAA
             , callEE
             , concurrently
             , evalAsync
             ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Data.Profunctor

type Async = ExceptT SomeException (ContT () IO)

now :: IO a -> Async a
now = lift . lift

async :: IO a -> Async a
async = ExceptT . ContT . fmap void . forkFinally

err :: SomeException -> Async a
err = ExceptT . lift . return . Left

concurrently :: Async a -> Async b -> Async (a, b)
concurrently a1 a2 = ExceptT $ lift $ do
  a1MVar <- newEmptyMVar
  a2MVar <- newEmptyMVar
  _ <- forkIO $ (runContT . runExceptT) a1 $ putMVar a1MVar
  _ <- forkIO $ (runContT . runExceptT) a2 $ putMVar a2MVar
  e1 <- takeMVar a1MVar
  e2 <- takeMVar a2MVar
  return $ (,) <$> e1 <*> e2

callAA :: ((a -> Async b) -> Async a) -> Async a
callAA = ExceptT . callCC . dimap (dimap Right ExceptT) runExceptT

callEE :: ((SomeException -> Async b) -> Async a) -> Async a
callEE = ExceptT . callCC . dimap (dimap Left ExceptT) runExceptT

evalAsync :: (SomeException -> IO ()) -> Async () -> IO ()
evalAsync f a = evalContT $ runExceptT a >>= (lift . g)
  where g (Left e) = f e
        g (Right ()) = return ()
