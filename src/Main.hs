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

import Color
import Animation

import qualified Proto as P

main :: IO ()
main = do
  let port = "/dev/ttyACM0"
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  replicateM_ 3 $ send s $ toStrict $ encode (0 :: Word64)
  frame <- fillRandom
  --runAnimation s 100000 (Animation cycleRight frame)
  void $ flip runStateT 0 $ runAnimation s 10 (Animation (sinBrightness frame) frame)
