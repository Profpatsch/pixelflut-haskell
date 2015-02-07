{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Pixelflut
       ( withConnection
       , Pixelflut
       , Color(..), rgb
       , black, white, red, green, blue, cyan, yellow, magenta
       , px, size, help, quit, text )
  where

import Control.Applicative ((<$>), Applicative)
import Control.Monad.State
import Control.Monad.Reader (ReaderT(..), MonadReader, ask)
import Data.Functor ()
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Data.List (stripPrefix, dropWhileEnd)
import Data.Word (Word8)
import Network.Socket  ( HostName, ServiceName, Socket
                       , getAddrInfo, AddrInfo(..), defaultHints
                       , socket, connect, close )
import Network.Socket.ByteString (sendAll, recv)
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as Char8


--------------------------------------------------------------------------------
newtype Pixelflut a = Pixelflut { runPixelflut :: ReaderT Socket IO a }
                      deriving ( Functor, Applicative, Monad
                               , MonadIO, MonadReader Socket)
                      
--------------------------------------------------------------------------------
data Color = RGBA Word8 Word8 Word8 Word8
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = RGBA r g b 0

black = rgb 0 0 0
red = rgb 255 0 0
green = rgb 0 255 0
blue = rgb 0 0 255
white = red <> green <> blue
yellow = red <> green
cyan = green <> blue
magenta = red <> blue  

instance Monoid Color where
  mempty = RGBA 0 0 0 0
  mappend (RGBA r g b a) (RGBA r' g' b' a') =
    (RGBA (r +++ r') (g +++ g') (b +++ b') (a +++ a'))
      where x +++ y = max 255 (x + y)

instance Show Color where
  show (RGBA r g b a) = printf "%.2x%.2x%.2x%.2x" r g b a

--------------------------------------------------------------------------------
startConnection :: HostName -> ServiceName -> IO Socket
startConnection host service = do
  i <- head <$> getAddrInfo (Just defaultHints) (Just host) (Just service)
  s <- socket (addrFamily i) (addrSocketType i) (addrProtocol i)
  connect s $ addrAddress i
  -- print "Connected!"
  return s

withConnection :: HostName -> ServiceName -> Pixelflut () -> IO ()
withConnection host service pf = do
  s <- startConnection host service
  (runReaderT.runPixelflut) pf s
  close s

--------------------------------------------------------------------------------
sendStr :: String -> Pixelflut ()
sendStr str = do
  s <- ask
  liftIO . sendAll s $ Char8.pack str

recvStr :: Pixelflut String
recvStr = do
  s <- ask
  -- 256 bytes should be enough (ugh).
  liftIO $ (recv s 256 >>= return . Char8.unpack)
  
--------------------------------------------------------------------------------
px :: Int -> Int -> Color -> Pixelflut ()
px x y (RGBA r g b _) = sendStr $ printf "PX %d %d %.2x%.2x%.2x\n" x y r g b

size :: Pixelflut (Maybe (Int, Int))
size = sendStr "SIZE\n" >> recvStr >>= return . parseSize
  where
    parseSize :: String -> Maybe (Int, Int)
    parseSize l = do
      l' <- stripPrefix "SIZE " $ dropWhileEnd (=='\n') l
      (x, y) <- return $ break (==' ') l'
      x' <- readMaybe x
      y' <- readMaybe y
      return (x', y')

help :: Pixelflut String
help = sendStr "HELP\n" >> recvStr >>= return

quit :: Pixelflut ()
quit = sendStr "QUIT\n"

text :: Int -> Int -> String -> Pixelflut ()
text x y t = sendStr $ printf "TEXT %d %d %s" x y t
