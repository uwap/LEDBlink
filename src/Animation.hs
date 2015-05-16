module Animation where

import Color
import System.Hardware.Serialport
import System.Random
import Control.Concurrent
import Control.Monad

import qualified Proto as P

type Frame = [Color]
type Animation = [Frame]

runAnimation :: SerialPort -> Int -> Animation -> IO ()
runAnimation _ _ [] = return ()
runAnimation s delay (frame:frames) = do
  P.perform s $ P.fill frame
  threadDelay delay
  runAnimation s delay frames

fill :: [Color] -> Animation
fill = return

cycleLeft :: Frame -> Animation
cycleLeft frame = fr : cycleLeft fr
              where
                fr = drop (length frame - 1) frame ++ take (length frame - 1) frame

cycleRight :: Frame -> Animation
cycleRight frame = fr : cycleRight fr
              where
                fr = drop 1 frame ++ take 1 frame

fillRandom :: IO Frame
fillRandom = replicateM 30 $ do
  r <- randomIO
  g <- randomIO
  b <- randomIO
  return (r,g,b)
