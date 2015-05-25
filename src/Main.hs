{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString as B (ByteString, null) 
import Data.ByteString.Lazy (toStrict)
import System.Hardware.Serialport
import System.Random
import Control.Concurrent
import Control.Monad.State
import Data.Binary
import Data.Binary.Put
import Data.Word
import Data.Foldable
import Data.Maybe
import Data.Int
import Data.Text (Text, unpack)
import Data.Monoid
import Text.Read
import qualified Network.WebSockets as WS

import Color
import Animation

import qualified Proto as P

defaultOr :: Maybe Color -> Color
defaultOr = fromMaybe (0,0,0)

fromMaybeColor :: (Color -> Animation) -> Maybe Color -> Animation
fromMaybeColor f c = f $ defaultOr c

main :: IO ()
main = do
    let port = "/dev/ttyACM0"
    s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
    replicateM_ 3 $ send s $ toStrict $ encode (0 :: Word64)
    animation <- newMVar (fillRandom <> sinBrightness <> cycleRight)
    forkIO $ WS.runServer "127.0.0.1" 8080 $ webSocketServer animation
    loop s animation Nothing 
  where
    loop :: SerialPort -> MVar Animation -> Maybe ThreadId -> IO ()
    loop s animation mtid = do
      anim <- takeMVar animation
      case mtid of
        Just tid -> killThread tid
        Nothing -> return ()
      tid <- forkIO $ animate s anim
      loop s animation (Just tid)

    readColor :: WS.Connection -> IO (Maybe Color)
    readColor conn = do
      r <- readMaybe . unpack <$> WS.receiveData conn
      g <- readMaybe . unpack <$> WS.receiveData conn
      b <- readMaybe . unpack <$> WS.receiveData conn
      return $ liftM3 (,,) r g b

    webSocketServer :: MVar Animation -> WS.PendingConnection -> IO ()
    webSocketServer animation req = do
      conn <- WS.acceptRequest req
      forever $ do
        dat <- WS.receiveData conn :: IO Text
        putMVar animation =<< case dat of
          "randomCycleRight" -> return $ fillRandom <> cycleRight 
          "randomSin"        -> return $ fillRandom <> sinBrightness
          "fillColor"        -> return . fromMaybeColor fillColor =<< readColor conn
          "colorSin"         -> (\color -> return (fromMaybeColor fillColor color <> sinBrightness)) =<< readColor conn
          "centered"         -> return . fromMaybeColor centerColor =<< readColor conn
          _                  -> return $ fillColor (0,0,0)
