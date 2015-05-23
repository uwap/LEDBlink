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
import Data.Text (Text)
import qualified Network.WebSockets as WS

import Color
import Animation

import qualified Proto as P

data Mode = Black | RandomCycleRight | FillColor Color | FillFrame Frame | ColorSin Color | RandomSin

main :: IO ()
main = do
    let port = "/dev/ttyACM0"
    s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
    replicateM_ 3 $ send s $ toStrict $ encode (0 :: Word64)
    mode <- newMVar Black
    forkIO $ WS.runServer "127.0.0.1" 8080 $ \req -> do
      conn <- WS.acceptRequest req
      forever $ do
        dat <- WS.receiveData conn :: IO Text
        case dat of
          "randomCycleRight" -> putMVar mode RandomCycleRight
          "randomSin" -> putMVar mode RandomSin
          _ -> putMVar mode Black
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

    readColor :: IO Color
    readColor = do
      r <- readLn
      g <- readLn
      b <- readLn
      return (r,g,b)

    animate s mode = do
      frame <- fromJust <$> fillRandom
      liftIO $ case mode of
        Black            -> runAnimationOnce s 0 (fill (replicate 30 (0,0,0)))
        RandomCycleRight -> runAnimation s 100000 (Animation cycleRight frame)
        FillColor color  -> runAnimationOnce s 0 (fill (replicate 30 color))
        FillFrame frame  -> runAnimationOnce s 0 (fill frame)
        ColorSin color   -> forever $ flip runStateT 0 $
          runAnimation s 10000 (Animation (sinBrightness (replicate 30 color)) (replicate 30 (0,0,0)))
        RandomSin        -> forever $ flip runStateT 0 $ do
          frame <- liftIO $ fromJust <$> fillRandom
          runAnimation s 10000 (Animation (sinBrightness frame) (replicate 30 (0,0,0)))
