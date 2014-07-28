{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Binary as Bi
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import           Network.Simple.TCP


toStrict :: L.ByteString -> S.ByteString
toStrict = mconcat . L.toChunks 

toLazy :: S.ByteString -> L.ByteString 
toLazy x = L.fromChunks [x]



packAndSend :: (Bi.Binary a) => Socket -> a -> IO ()
packAndSend sock itm = do
    let bmsg = (toStrict . Bi.encode) itm
        sz :: Bi.Word32 = fromIntegral (S.length bmsg)
        sz_bstr = (toStrict . Bi.encode) sz
    send sock sz_bstr
    send sock bmsg

recvAndUnpack :: (MonadIO m, Bi.Binary a) => Socket -> MaybeT m a
recvAndUnpack sock = do 
    sz_bstr <- MaybeT . liftIO $ recv sock 4
    let sz :: Bi.Word32 = (Bi.decode . toLazy) sz_bstr
    str <- MaybeT . liftIO $ recv sock (fromIntegral sz)
    return $ (Bi.decode . toLazy) str


