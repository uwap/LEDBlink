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

sendFrame :: SerialPort -> [Color] -> IO ()
sendFrame s frame = send' (zip [0..] frame)
  where
    -- Send only 30 pixels at once
    send' [] = do
      send s $ toStrict $ runPut $ do
        showPixels
        put (99 :: Word8)
      waitForAcknowledgement
      return ()
    send' list = do
      send s $ toStrict $ runPut $ setPixels (take 30 list)
      waitForAcknowledgement
      send' (drop 30 list)

    waitForAcknowledgement = loop 0

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
