{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (forever)
import           Control.Monad.Loops
import           Network.Simple.TCP
--
import           Common

messagequeue :: TVar [Message] -> TVar [Message] -> IO ()
messagequeue tvar tvarLog = forever $ atomically $ do 
    bufmsgs <-readTVar tvar
    if null bufmsgs 
      then retry 
      else do 
        writeTVar tvar []
        logs <- readTVar tvarLog
        let newlogs = bufmsgs ++ logs
        writeTVar tvarLog newlogs 

main :: IO ()
main = do 
  putStrLn " I am server " 
  tvar <- atomically $ newTVar ([] :: [Message] )
  tvarLog <- atomically $ newTVar ([] :: [Message])
  forkIO $ messagequeue tvar tvarLog

  forkIO $ serve HostAny "5002" $ \(sock,addr) -> do 
    putStrLn $ "TCP connection established from " ++ show addr
    let go n = do (n',logs) <- atomically $ do
                                 logs <- readTVar tvarLog
                                 let n' = length logs
                                 if length logs == n 
                                   then retry
                                   else return (n',logs)
                  print logs
                  packAndSend sock logs 
                  go n'
    go 0

  serve HostAny "5003" $ \(sock,addr) -> unfoldM_ $ do 
    putStrLn $ "Getting message from " ++ show addr
    mtxt :: Maybe Message <- recvAndUnpack sock
    case mtxt of 
      Nothing -> return () 
      Just txt -> do putStrLn $ "got message : " ++ show txt
                     atomically $ do 
                       txts <- readTVar tvar
                       writeTVar tvar (txt:txts)
    return mtxt
  -- 
  getLine
  return ()
