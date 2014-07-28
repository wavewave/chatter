{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent 
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Data.Binary as Bi
import qualified Data.ByteString.Lazy as BWL

-- import Network
import Network.Simple.TCP
import System.Environment
import System.IO

tick :: IO ()
tick = do 
  putStrLn "tick" 
  threadDelay 1000000
  tick 

-- bufSize = 10 -- 1024 

client :: String -> IO ()
client addr = do
  connect addr "5002" $ \(sock, remoteAddr) -> do
    putStrLn $ "Client: Connection established to " ++ show remoteAddr 
    forever $ do 
      runMaybeT $ do 
        sz_bstr <- MaybeT $ recv sock 4
        let sz :: Bi.Word32 = Bi.decode (BWL.fromChunks [sz_bstr]) 
        str <- MaybeT $ recv sock (fromIntegral sz) 
        liftIO $ print str 
      return ()


main :: IO ()
main = do 
  putStrLn "chatting client" 
  args <- getArgs
  client (args !! 0) -- >> threadDelay 1000000
 

