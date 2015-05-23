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

main :: IO ()
main = do
    let port = "/dev/ttyACM0"
    s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
    replicateM_ 3 $ send s $ toStrict $ encode (0 :: Word64)
    mode <- readLn
    loop s mode 
  where
    loop s mode = do
      tid <- forkIO $ animate s mode
      nmode <- readLn
      killThread tid
      loop s nmode

    animate s mode = do
      frame <- fromJust <$> fillRandom
      liftIO $ case mode of
        0 -> runAnimation s 100000 (Animation cycleRight frame)
        1 -> forever $ flip runStateT 0 $ do
          frame <- liftIO $ fromJust <$> fillRandom
          runAnimation s 10000 (Animation (sinBrightness frame) (replicate 30 (0,0,0)))
