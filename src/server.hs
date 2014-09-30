{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Text as T
import           Data.Text.Binary
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

broadcaster :: TVar [Message] -> IO ()
broadcaster tvarLog = do
  serve HostAny "5002" $ \(sock,addr) -> do 
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



main :: IO ()
main = do 
  putStrLn " I am server " 
  tvar <- atomically $ newTVar ([] :: [Message] )
  tvarLog <- atomically $ newTVar ([] :: [Message])
  forkIO $ messagequeue tvar tvarLog
  forkIO $ broadcaster tvarLog

  serve HostAny "5003" $ \(sock,addr) -> do  
    putStrLn $ "Getting message from " ++ show addr
    flip evalStateT 0 $ 
      whileJust_ (lift (recvAndUnpack sock)) $ \txt -> do
        liftIO $ putStrLn $ "got message : " ++ T.unpack txt
        registerMessage tvar txt
  -- 
  getLine
  return ()

registerMessage :: TVar [Message] -> T.Text -> StateT Int IO ()
registerMessage tvar txt = do
    n <- get
    liftIO $ atomically $ do 
      txts <- readTVar tvar
      writeTVar tvar ((Message n txt):txts)
    put (n+1)
