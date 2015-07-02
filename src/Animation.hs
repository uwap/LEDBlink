module Animation where

import Color
import System.Hardware.Serialport
import System.Random
import Control.Monad
import Control.Lens
import Data.IORef

import Proto (sendFrame)

pixels :: Int
pixels = 30

type Frame = [Color]
type Animation = Frame -> IO Frame
type Counter = IORef Int

animate :: SerialPort -> Counter -> Frame -> Animation -> IO ()
animate s counter startFrame ani = runAnimation startFrame
  where
    runAnimation :: Frame -> IO ()
    runAnimation frame = do
      _ <- sendFrame s frame
      modifyIORef counter (+1)
      newFrame <- ani frame
      runAnimation newFrame

every :: Int -> Counter -> Animation -> Animation
every i counter f frame = do
  j <- readIORef counter
  if i `mod` j == 0 then
    f frame
  else
    return frame

fill :: Frame -> Animation
fill frame _ = return frame

fillColor :: Color -> Animation
fillColor color _ = return $ replicate pixels color

cycleLeft :: Animation
cycleLeft frame = return (last frame : init frame)

cycleRight :: Animation
cycleRight frame = return (tail frame ++ [head frame])

{-center :: Animation
center frame = return (loop pixels)
  where 
    center :: Color -> Int -> Int
    center col 0 = 0
    center col i = fromIntegral ((round(fromIntegral pixels / 2) - abs(round(fromIntegral pixels / 2) - i)) ^ 2)
-}
fillRandom :: Animation
fillRandom _ = randomFrame

randomFrame :: IO Frame
randomFrame = replicateM pixels randomColor
  where
    randomColor = Color 1 <$> randomIO <*> randomIO <*> randomIO

sinBrightness :: Counter -> Animation
sinBrightness counter frame = do
  i <- readIORef counter
  let deltaB = sinFactor i - sinFactor (i-1)
  return ((& brightness %~ (+) deltaB) <$> frame)

{-
addSin :: Counter -> Color -> Animation
addSin counter col frame = do
  i <- readIORef counter
  let brightness = flip setBrightness (sinFactor i) <$> replicate pixels (255,255,255)
  let sin = flip setBrightness (sinFactor i) <$> (*col) <$> fromIntegral <$> [min (3-abs(i - j)) 0 * 80 | j <- [1..pixels]]
  return (uncurry (+) <$> zip frame sin)
-}
sinFactor :: Int -> Double
sinFactor i = 1 - abs (sin ((fromIntegral i / 100) + 3.1415926535/2))
