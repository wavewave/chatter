{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (forever,guard)
import           Control.Monad.IO.Class
import           Control.Monad.Loops 
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.Text as T
import           Data.Text.Binary
import           Network.Simple.TCP
import           System.Environment
--
import           Common

data Command = ViewAfter Int 

commander :: TVar [Message] -> Command -> IO ()
commander logvar (ViewAfter n) = do
    xs <- atomically (readTVar logvar) 
    (mapM_ (putStrLn . prettyPrintMessage) 
     . sortBy (compare `on` messageNum)
     . filter ( (> n) . messageNum ) 
     ) xs

addLog :: TVar [Message] -> [Message] -> IO ()
addLog logvar msgs = atomically $ do 
                       log <- readTVar logvar
                       writeTVar logvar (log ++ msgs) 
 

clientReceiver :: TVar [Message] -> String -> String -> IO ()
clientReceiver logvar ipaddrstr username = 
  connect ipaddrstr "5002" $ \(sock,addr) -> do 
    putStrLn $ "Connection established to " ++ show addr
    flip evalStateT 0 $ whileJust_  (lift (recvAndUnpack sock)) $ \xs -> 
      if null xs 
        then return ()
        else do n <- get
                let n' = checkLatestMessage xs
                liftIO $ addLog logvar xs
                liftIO $ commander logvar (ViewAfter n)
                put n'
                              

clientSender :: String -> String -> IO ()
clientSender ipaddrstr username = 
  connect ipaddrstr "5003" $ \(sock,_addr) -> forever $ do
    str <- getLine :: IO String
    packAndSend sock (T.pack username, T.pack str)


main :: IO ()
main = do 
  args <- getArgs
  guard (length args == 2)
  putStrLn " i am client " 
  logvar <- atomically $ newTVar []

  forkIO $ clientReceiver logvar (args !! 0) (args !! 1)
  clientSender (args !! 0) (args !! 1)

        
