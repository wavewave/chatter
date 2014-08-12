
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

transactionA :: (TVar Int, TVar Int) -> STM (Int,Int)
transactionA (aref,bref) = do a <- readTVar aref 
                              b <- readTVar bref
                              if b < 10 then retry else do  
                                writeTVar aref (a+10)
                                writeTVar bref (b-10)
                              return (a,b)


transactionA' :: (TVar Int, TVar Int) -> STM (Int,Int)
transactionA' (aref,bref) = do a <- readTVar aref 
                               b <- readTVar bref
                               if a < 30 then retry else do 
                                 writeTVar aref (a-30)
                                 writeTVar bref (b+30)
                               return (a,b)


transactionB :: (TVar Int, TVar Int) -> STM (Int,Int)
transactionB (aref,bref) = do a <- readTVar aref 
                              b <- readTVar bref
                              writeTVar aref (a-1)
                              writeTVar bref (b+1)
                              return (a,b)


threadA :: (TVar Int, TVar Int) -> IO ()
threadA (aref,bref) = do
    (a,b) <- atomically (transactionA (aref,bref) `orElse` transactionA' (aref,bref) ) 
    putStrLn $ "threadA: " ++ show a ++ ", " ++ show b
    threadDelay 10000

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
