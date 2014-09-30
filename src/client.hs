{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent (forkIO)
import           Control.Monad (forever)
import qualified Data.Text as T
import           Network.Simple.TCP
--
import           Common


main :: IO ()
main = do 
  putStrLn " i am client " 

  forkIO $ connect "127.0.0.1" "5002" $ \(sock,addr) -> do 
    putStrLn $ "Connection established to " ++ show addr
    forever $ do
      x :: Maybe [Message] <- recvAndUnpack sock 
      putStrLn $ "server sent : " ++ show x

  connect "127.0.0.1" "5003" $ \(sock,_addr) -> forever $ do
    str <- getLine :: IO String
    packAndSend sock (Message 10 (T.pack str))

        
