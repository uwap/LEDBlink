module Animation where

import Color
import System.Hardware.Serialport
import System.Random
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import System.IO.Unsafe

import qualified Proto as P

type Frame = [Color]
type AnimationGenerator = Integer -> Frame -> IO Frame
type Animation = [AnimationGenerator]

animate :: SerialPort -> Frame -> Animation -> IO ()
animate s startFrame ani = combineAnimations 0 startFrame ani
  where
    combineAnimations :: Integer -> Frame -> Animation -> IO ()
    combineAnimations i frame a = do
      f <- combineAll i frame a
      P.perform s $ P.fill (join $ replicate 3 f)
      combineAnimations (i+1) frame ani 

combineAll :: Integer -> Frame -> Animation -> IO Frame
combineAll i frame (step:[]) = step i frame
combineAll i frame (step:steps) = do
  f <- step i frame
  combineAll i f steps

every :: Integer -> Animation -> Animation
every i ani = return step
  where
    step j frame = combineAll (floor (fromIntegral j / fromIntegral i)) frame ani

fill :: Frame -> Animation
fill frame = return $ const $ const $ return frame

fillColor :: Color -> Animation
fillColor color = return $ const $ const $ return $ replicate 30 color

cycleLeft :: Animation
cycleLeft = return (\i -> step (i `mod` 300))
  where
    step 0 frame = return (last frame : init frame)
    step i frame | i `mod` 10 == 0 = step (i-1) (last frame : init frame)
    step i frame = step (i-1) frame

cycleRight :: Animation
cycleRight = return (\i -> step (i `mod` 300))
  where
    step 0 frame = return $ tail frame ++ [head frame]
    step i frame | i `mod` 10 == 0 = step (i-1) $ tail frame ++ [head frame]
    step i frame = step (i-1) frame

centerColor :: Color -> Animation
centerColor color = return step
  where 
    step _ _ = return $ center color 30
    
    center :: Color -> Int -> Frame
    center col 0 = [(0,0,0)]
    center col i = (fromIntegral ((15 - abs(15 - i)) ^ 2) * col) : center col (i -1)

fillRandom :: Animation
fillRandom = return step
  where step _ _ = randomFrame

randomFrame :: IO Frame
randomFrame = replicateM 30 $ liftM3 (,,) randomIO randomIO randomIO

sinBrightness :: Animation
sinBrightness = return step
  where
    step i frame = let factor = 1 - abs (sin ((fromIntegral i / 100) + 3.1415926535/2)) in
      return $ fmap (uncurry (*)) $ zip frame $ flip setBrightness factor <$> replicate 30 (255,255,255)

addSin :: Int -> Color -> Animation
addSin i col = return step
  where
    step j frame = let factor = 1 - abs (sin ((fromIntegral i / 100) + 3.1415926535/2)) in
      return $ fmap (uncurry (+)) $ zip frame $ flip setBrightness factor <$> createSin
    createSin = (*col) <$> fromIntegral <$> [(min (3-abs(i - j)) 0) * 80 | j <- [1..30]]
