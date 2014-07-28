{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent 

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe



import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.Simple.TCP
import System.Environment

--
import Message
import Util

formatMessage :: Message -> T.Text
formatMessage msg = T.pack (show (lineno msg)) <> ": " <> msgbody msg

client :: String -> IO ()
client addr = do 
    forkIO $ connect addr "5002" $ \(sock, servaddr) -> do
      putStrLn $ "Client: Connection established to " ++ show servaddr
      runMaybeT $ clientWorker sock (-1)
      return ()
    connect addr "5003" $ \(sock, servaddr) -> do
      putStrLn $ "Client: Connection established to " ++ show servaddr
      inputlineWorker sock
    return () 
  where clientWorker sock n = do 
          liftIO $ packAndSend sock n
          msgs :: [Message] <- recvAndUnpack sock 
          mapM_ (liftIO . TIO.putStrLn . formatMessage) (reverse msgs)
          if ((not . null) msgs) 
            then clientWorker sock ((lineno . head) msgs)
            else clientWorker sock (-1)
        inputlineWorker sock = do
          txt <- TIO.getLine
          packAndSend sock (TE.encodeUtf8 txt)
          inputlineWorker sock

main :: IO ()
main = do 
  putStrLn "chatting client" 
  args <- getArgs
  client (args !! 0)
 

