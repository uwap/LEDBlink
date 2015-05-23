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

import Color
import Animation

import qualified Proto as P

data Mode = Black | RandomCycleRight | FillColor Color | FillFrame Frame | ColorSin Color | RandomSin

main :: IO ()
main = do
    let port = "/dev/ttyACM0"
    s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
    replicateM_ 3 $ send s $ toStrict $ encode (0 :: Word64)
    mode <- readLn
    loop s mode 
  where
    loop s mode = do
      action <- case mode of
        1 -> return RandomCycleRight
        2 -> return RandomSin
        3 -> return . FillColor =<< readColor
        4 -> return . ColorSin =<< readColor
        _ -> return Black
      tid <- forkIO $ animate s action
      nmode <- readLn
      killThread tid
      loop s nmode

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
