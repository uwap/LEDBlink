{-# LANGUAGE OverloadedStrings #-}
module Proto where

import Data.Binary
import Data.Binary.Put
import Data.ByteString as B (ByteString, null) 
import Data.ByteString.Lazy (toStrict)
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad
import Color

perform :: SerialPort -> Put -> IO ByteString
perform s p = do
    send s $ toStrict $ runPut p
    loop 0
  where
    loop i = do
      str <- recv s 1
      if B.null str then do
        threadDelay 10
        if i > 10 then do
          setDTR s True
          threadDelay 1000
          setDTR s False
          threadDelay 5000000
          return ""
        else
          loop (i + 1)
      else
        return str

fill :: [Color] -> Put
fill colors = do
    setPixels $ zip [0..] colors
    showPixels

showPixels :: Put
showPixels = put (3 :: Word8)

setPixels :: [(Word16, Color)] -> Put
setPixels list = do
  setMode 1
  setPixelLength (fromIntegral $ length list)
  forM_ list put

setMode :: Word8 -> Put
setMode = put

setPixelLength :: Word16 -> Put
setPixelLength = put
