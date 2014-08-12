
import Control.Concurrent
import Control.Monad
import Data.IORef

threadA :: (IORef Int, IORef Int) -> IO ()
threadA (aref,bref) = do
    a <- readIORef aref
    b <- readIORef bref
    putStrLn $ "threadA:" ++ show a ++ ", " ++ show b

    -- threadDelay 100000

    writeIORef aref (a+1)
    writeIORef bref (b-1)


threadB :: (IORef Int, IORef Int) -> IO ()
threadB (aref,bref) = do
    a <- readIORef aref
    threadDelay 100

    b <- readIORef bref
    putStrLn $ "threadB:" ++ show a ++ ", " ++ show b
    

    writeIORef aref (a-1)
    writeIORef bref (b+1)

main :: IO ()
main = do
  aref <- newIORef (100 :: Int)
  bref <- newIORef (100 :: Int) 

  forkIO $ replicateM_ 1000 (threadA (aref,bref))

  replicateM_ 1000 (threadB (aref,bref))

  return ()
