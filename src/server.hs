{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Binary as Bi
import qualified Data.ByteString as BW
import qualified Data.ByteString.Lazy as BWL 
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import           Data.Monoid 
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word as W
-- import Network
import Network.Simple.TCP
import System.IO
-- 
import Message
import Util


main :: IO ()
main = do 
  putStrLn "chatting program server" 
  tvar <- atomically (newTVar [])
  server tvar
  return ()


server :: TVar [Message] -> IO () 
server tvar = do 
  -- 
  forkIO $ serve (Host "127.0.0.1") "5002" $ \(sock, remoteAddr) -> do
    putStrLn $ "Server: TCP connection established from " ++ show remoteAddr 
    forever $ do
      r <- runMaybeT $ recvAndUnpack sock 
      case r of
        Nothing -> return ()
        Just lastnum -> do
          msgs <- atomically $ do 
                    lst <- readTVar tvar
                    let lst' = filter ((>lastnum) .  lineno) lst
                    if null lst' then retry else return lst'
          packAndSend sock msgs
  serve (Host "127.0.0.1") "5003" $ \(sock, remoteAddr) -> do
    putStrLn $ "Server: TCP connection established from " ++ show remoteAddr 
    forever $ do
      r <- runMaybeT $ recvAndUnpack sock
      case r of
        Nothing -> return ()
        Just bmsg -> do
          let msg = TE.decodeUtf8 bmsg
          atomically $ do lst <- readTVar tvar
                          case lst of
                            y:ys -> let n = lineno y + 1
                                    in writeTVar tvar (Message n msg : lst)
                            [] -> writeTVar tvar [Message 0 msg] 
    

