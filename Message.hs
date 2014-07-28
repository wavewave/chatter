{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Message where 

import Data.Binary 
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Message = Message { lineno :: Int
                       , msgbody :: T.Text }
             deriving Show

instance Binary Message where
  put :: Message -> Put
  put Message {..} = put lineno >> put (TE.encodeUtf8 msgbody)

  get :: Get Message
  get = do n <- get
           bdy_bstr <- get
           let bdy = TE.decodeUtf8 bdy_bstr
           return (Message n bdy)

  
