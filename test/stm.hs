
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

transactionA :: (TVar Int, TVar Int) -> STM (Int,Int)
transactionA (aref,bref) = do a <- readTVar aref 
                              b <- readTVar bref
                              writeTVar aref (a+1)
                              writeTVar bref (b-1)
                              return (a,b)


transactionB :: (TVar Int, TVar Int) -> STM (Int,Int)
transactionB (aref,bref) = do a <- readTVar aref 
                              b <- readTVar bref
                              writeTVar aref (a-1)
                              writeTVar bref (b+1)
                              return (a,b)


threadA :: (TVar Int, TVar Int) -> IO ()
threadA (aref,bref) = do
    (a,b) <- atomically (transactionA (aref,bref))
    putStrLn $ "threadA: " ++ show a ++ ", " ++ show b
    threadDelay 1000

threadB :: (TVar Int, TVar Int) -> IO ()
threadB (aref,bref) = do
    (a,b) <- atomically (transactionB (aref,bref))
    putStrLn $ "threadB: " ++ show a ++ ", " ++ show  b
    threadDelay 2000

main :: IO ()
main = do
  (aref,bref) <- atomically $ do aref <- newTVar (100 :: Int)
                                 bref <- newTVar (100 :: Int) 
                                 return (aref,bref)

  forkIO $ replicateM_ 1000 (threadA (aref,bref))

  replicateM_ 1000 (threadB (aref,bref))

  return ()
