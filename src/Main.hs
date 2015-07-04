{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy (toStrict)
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad.State
import Data.Binary
import Data.IORef

import Animation
import Color
import qualified Websocket as WS

main :: IO ()
main = do
    let port = "/dev/ttyACM0"
    s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
    replicateM_ 3 $ send s $ toStrict $ encode (0 :: Word64)
    counter <- newIORef 0
    animation <- newMVar (sinBrightness counter >=> every 5 counter cycleRight)
    forkIO $ WS.start animation counter
    loop s animation counter Nothing 
  where
    loop :: SerialPort -> MVar Animation -> Counter -> Maybe ThreadId -> IO ()
    loop s animation counter mtid = do
      anim <- takeMVar animation
      case mtid of
        Just tid -> killThread tid
        Nothing -> return ()
      frame <- randomFrame
      tid <- forkIO $ animate s counter frame anim
      loop s animation counter (Just tid)
