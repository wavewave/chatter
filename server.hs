{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent 
import Control.Monad
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

tick :: IO ()
tick = do 
  putStrLn "tick" 
  threadDelay 1000000
  tick 


main :: IO ()
main = do 
  putStrLn "chatting program server" 
  server
  return ()


server :: IO () 
server = do 
  serve (Host "127.0.0.1") "5002" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Server: TCP connection established from " ++ show remoteAddr 

    F.forM_ [1..] $ \n -> do 
      threadDelay 1000000
      let txtmsg = "mesg " <> T.pack (show n) <> ": hello. I am server" 
          bmsg = TE.encodeUtf8 txtmsg
          sz :: Word32 = fromIntegral (B.length bmsg)
          sz_bstr = (mconcat . BWL.toChunks . Bi.encode) sz 
      print sz
      send connectionSocket sz_bstr
      send connectionSocket bmsg




