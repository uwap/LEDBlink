module Animation where

import Color
import System.Hardware.Serialport
import System.Random
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import qualified Proto as P

type Frame = [Color]
data AnimationGenerator = AnimationGenerator { step :: Integer -> Frame -> IO Frame, combinator :: Color -> Color -> Color }
type Animation = [AnimationGenerator]

animate :: SerialPort -> Animation -> IO ()
animate s ani = combineAnimations 0 (replicate 30 (0,0,0)) ani
  where
    combineAnimations :: Integer -> Frame -> Animation -> IO ()
    combineAnimations i frame (a:[]) = do
      f <- step a i frame
      P.perform s $ P.fill f
      combineAnimations (i+1) f ani 
    combineAnimations i frame (a:b:xs) = do
      af <- step a i frame
      bf <- step b i frame
      combineAnimations i frame ((AnimationGenerator (\_ _ -> return (uncurry (combinator b) <$> zip af bf)) (combinator b)) : xs)

fill :: Frame -> Animation
fill frame = return $ AnimationGenerator (const $ const $ return frame) (*)

fillColor :: Color -> Animation
fillColor color = return $ AnimationGenerator (const $ const $ return $ replicate 30 color) (*)

cycleLeft :: Animation
cycleLeft = return $ AnimationGenerator step const
  where step _ frame = return $ drop (length frame - 1) frame ++ take (length frame - 1) frame

cycleRight :: Animation
cycleRight = return $ AnimationGenerator step const
  where step _ frame = return $ drop 1 frame ++ take 1 frame

centerColor :: Color -> Animation
centerColor color = return $ AnimationGenerator step (+)
  where 
    step _ _ = return $ center color 30
    
    center :: Color -> Int -> Frame
    center col 0 = [(0,0,0)]
    center col i = (fromIntegral ((15 - abs(15 - i)) ^ 2) * col) : center col (i -1)

fillRandom :: Animation
fillRandom = return $ AnimationGenerator step (*)
  where step _ _ = replicateM 30 $ do
                    r <- randomIO
                    g <- randomIO
                    b <- randomIO
                    return (r,g,b)

sinBrightness :: Animation
sinBrightness = return $ AnimationGenerator step (*)
  where
    step i _ = let factor = 1 - abs (sin ((fromIntegral i / 100) + 3.1415926535/2)) in
      return $ flip setBrightness factor <$> replicate 30 (255,255,255)
