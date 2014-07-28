{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent 
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Binary as Bi
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BWL
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.Simple.TCP
import System.Environment
import System.IO
--
import Message
import Util

formatMessage :: Message -> T.Text
formatMessage msg = T.pack (show (lineno msg)) <> ": " <> msgbody msg

client :: String -> IO ()
client addr = do 
    forkIO $ connect addr "5002" $ \(sock, addr) -> do
      putStrLn $ "Client: Connection established to " ++ show addr
      runMaybeT $ clientWorker sock (-1)
      return ()
    connect addr "5003" $ \(sock, addr) -> do
      putStrLn $ "Client: Connection established to " ++ show addr
      inputlineWorker sock
    return () 
  where clientWorker sock n = do 
          liftIO $ packAndSend sock n
          msgs :: [Message] <- recvAndUnpack sock 
          mapM_ (liftIO . TIO.putStrLn . formatMessage) (reverse msgs)
          if ((not . null) msgs) 
            then clientWorker sock ((lineno . head) msgs)
            else clientWorker sock 0
        inputlineWorker sock = do
          txt <- TIO.getLine
          packAndSend sock (TE.encodeUtf8 txt)
          inputlineWorker sock

main :: IO ()
main = do 
  putStrLn "chatting client" 
  args <- getArgs
  client (args !! 0)
 

