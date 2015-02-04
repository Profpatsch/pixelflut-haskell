{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Pixelflut where

import Data.Word (Word8)
import qualified Data.ByteString.Char8 as Char8
import Network.Socket  ( HostName, ServiceName, Socket
                       , getAddrInfo, AddrInfo(..), defaultHints
                       , socket, connect )
import Network.Socket.ByteString (sendAll)
import Data.Functor ()
import Control.Applicative ((<$>), Applicative)
import Control.Monad.State
import Control.Monad.Reader (ReaderT, MonadReader, ask)
import Text.Printf (printf)


--------------------------------------------------------------------------------
newtype Pixelflut a = Pixelflut { runPixelflut :: ReaderT Socket IO a }
                      deriving (Functor, Applicative, Monad
                               , MonadIO, MonadReader Socket)
                      

data Color = RGBA Word8 Word8 Word8 Word8
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = RGBA r g b 0

instance Show Color where
  show (RGBA r g b a) = printf "%x%x%x%x" r g b a

--------------------------------------------------------------------------------
withConnection :: HostName -> ServiceName -> IO Socket
withConnection host service = do
  i <- head <$> getAddrInfo (Just defaultHints) (Just host) (Just service)
  s <-  socket (addrFamily i) (addrSocketType i) (addrProtocol i)
  connect s $ addrAddress i
  return s

--------------------------------------------------------------------------------
px :: Int -> Int -> Color -> Pixelflut ()
px x y (RGBA r g b a) = do
  s <- ask
  liftIO $ sendAll s $ Char8.pack $ printf "PX %d %d %x%x%x%x" x y r g b a
