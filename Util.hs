module Util where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Monoid


toStrict :: L.ByteString -> S.ByteString
toStrict = mconcat . L.toChunks 

toLazy :: S.ByteString -> L.ByteString 
toLazy x = L.fromChunks [x]
