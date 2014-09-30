{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent (forkIO)
import           Control.Monad (forever)
import           Control.Monad.Loops 
import qualified Data.Text as T
import           Data.Text.Binary
import           Network.Simple.TCP
--
import           Common


main :: IO ()
main = do 
  putStrLn " i am client " 

  forkIO $ connect "127.0.0.1" "5002" $ \(sock,addr) -> do 
    putStrLn $ "Connection established to " ++ show addr
    whileJust_  (recvAndUnpack sock) $ \xs -> do
      mapM_ (putStrLn . prettyPrintMessage) xs


  connect "127.0.0.1" "5003" $ \(sock,_addr) -> forever $ do
    str <- getLine :: IO String
    packAndSend sock (T.pack str) -- (Message 10 (T.pack str))

        
