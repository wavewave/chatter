{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent 
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Data.Binary as Bi
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BWL

-- import Network
import Network.Simple.TCP
import System.Environment
import System.IO
--
import Message
import Util

tick :: IO ()
tick = do 
  putStrLn "tick" 
  threadDelay 1000000
  tick 

-- bufSize = 10 -- 1024 

client :: String -> IO ()
client addr = 
  do          putStrLn "hello" 
              
              connect addr "5002" $ \(sock, remoteAddr) -> do
                putStrLn $ "Client: Connection established to " ++ show remoteAddr
                runMaybeT $ clientWorker sock (-1)
                return ()
  where clientWorker sock n = do 
          let bmsg' = (toStrict . Bi.encode) n
              sz' :: Bi.Word32 = fromIntegral (B.length bmsg')
              sz_bstr' = (toStrict . Bi.encode) sz'
          liftIO (send sock sz_bstr')
          liftIO (send sock bmsg')
          -- 
          sz_bstr <- MaybeT $ recv sock 4
          let sz :: Bi.Word32 = (Bi.decode . toLazy) sz_bstr
          str <- MaybeT $ recv sock (fromIntegral sz)
          let msg :: [Message] = (Bi.decode . toLazy) str
          liftIO $ print msg
          if ((not . null) msg) 
            then clientWorker sock ((lineno . head) msg)
            else clientWorker sock 0
          -- return ()


main :: IO ()
main = do 
  putStrLn "chatting client" 
  args <- getArgs
  client (args !! 0) -- >> threadDelay 1000000
 

