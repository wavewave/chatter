{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent (forkIO)
import           Control.Monad (forever,guard)
import           Control.Monad.Loops 
import qualified Data.Text as T
import           Data.Text.Binary
import           Network.Simple.TCP
import           System.Environment
--
import           Common

clientReceiver :: String -> String -> IO ()
clientReceiver ipaddrstr username = 
  connect ipaddrstr "5002" $ \(sock,addr) -> do 
    putStrLn $ "Connection established to " ++ show addr
    whileJust_  (recvAndUnpack sock) $ \xs -> do
      (mapM_ (putStrLn . prettyPrintMessage) . reverse) xs

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
  forkIO $ clientReceiver (args !! 0) (args !! 1)
  clientSender (args !! 0) (args !! 1)

        
