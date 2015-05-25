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
import Text.Read
import qualified Network.WebSockets as WS

import Color
import Animation

import qualified Proto as P

data Mode = Black
          | FillColor Color
          | FillFrame Frame
          | RandomCycleRight
          | RandomSin
          | ColorSin Color
          | CenterSin Color
          | Centered Color

defaultMode :: Mode
defaultMode = Black

defaultOr :: Maybe Mode -> Mode
defaultOr = fromMaybe Black 

fromMaybeColor :: (Color -> Mode) -> Maybe Color -> Mode
fromMaybeColor mode col = defaultOr $ liftM mode col

main :: IO ()
main = do
    let port = "/dev/ttyACM0"
    s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
    replicateM_ 3 $ send s $ toStrict $ encode (0 :: Word64)
    mode <- newMVar Black
    forkIO $ WS.runServer "127.0.0.1" 8080 $ webSocketServer mode
    loop s mode Nothing 
  where
    loop :: SerialPort -> MVar Mode -> Maybe ThreadId -> IO ()
    loop s mode mtid = do
      action <- takeMVar mode
      case mtid of
        Just tid -> killThread tid
        Nothing -> return ()
      tid <- forkIO $ animate s action
      loop s mode (Just tid)

    readColor :: WS.Connection -> IO (Maybe Color)
    readColor conn = do
      r <- readMaybe . unpack <$> WS.receiveData conn
      g <- readMaybe . unpack <$> WS.receiveData conn
      b <- readMaybe . unpack <$> WS.receiveData conn
      return $ liftM3 (,,) r g b

    webSocketServer :: MVar Mode -> WS.PendingConnection -> IO ()
    webSocketServer mode req = do
      conn <- WS.acceptRequest req
      forever $ do
        dat <- WS.receiveData conn :: IO Text
        putMVar mode =<< case dat of
          "randomCycleRight" -> return RandomCycleRight
          "randomSin"        -> return RandomSin
          "fillColor"        -> return . fromMaybeColor FillColor =<< readColor conn
          "colorSin"         -> return . fromMaybeColor ColorSin =<< readColor conn
          "centered"         -> return . fromMaybeColor Centered =<< readColor conn
          "centerSin"        -> return . fromMaybeColor CenterSin =<< readColor conn
          _                  -> return Black

    animate :: SerialPort -> Mode -> IO ()
    animate s mode = do
      frame <- fromJust <$> fillRandom
      case mode of
        Black            -> runAnimationOnce s 0 (fill (replicate 30 (0,0,0)))
        RandomCycleRight -> runAnimation s 100000 (Animation cycleRight frame)
        FillColor color  -> runAnimationOnce s 0 (fill (replicate 30 color))
        FillFrame frame  -> runAnimationOnce s 0 (fill frame)
        Centered color   -> runAnimationOnce s 0 (fill $ center color 30)
        CenterSin color  -> forever $ flip runStateT 0 $
          runAnimation s 10000 (Animation (sinBrightness (center color 30)) (replicate 30 (0,0,0)))
        ColorSin color   -> forever $ flip runStateT 0 $
          runAnimation s 10000 (Animation (sinBrightness (replicate 30 color)) (replicate 30 (0,0,0)))
        RandomSin        -> forever $ flip runStateT 0 $ do
          frame <- liftIO $ fromJust <$> fillRandom
          runAnimation s 10000 (Animation (sinBrightness frame) (replicate 30 (0,0,0)))

    center :: Color -> Int -> Frame
    center col 0 = [(0,0,0)]
    center col i = (fromIntegral ((15 - abs(15 - i)) ^ 2) * col) : center col (i -1)
