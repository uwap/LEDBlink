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
-- An animation consists out of a step function and a start value
data Animation m = Animation { step :: Frame -> m Frame, startFrame :: Frame }

type StateIO a b = StateT a IO b

runAnimation :: MonadIO m => SerialPort -> Int -> Animation m -> m ()
runAnimation s delay animation = runAnim (startFrame animation)
  where
    runAnim frame = do
      liftIO $ do
        P.perform s (P.fill frame)
        threadDelay delay
      runAnim =<< step animation frame

fill :: Monad m => Frame -> Animation m
fill frame = Animation return frame

cycleLeft :: MonadIO m => Frame -> m Frame
cycleLeft frame = liftIO $ return (drop (length frame - 1) frame ++ take (length frame - 1) frame)

cycleRight :: MonadIO m => Frame -> m Frame
cycleRight frame = liftIO $ return (drop 1 frame ++ take 1 frame)

fillRandom :: MonadIO m => m Frame
fillRandom = liftIO $ replicateM 30 $ do
  r <- randomIO
  g <- randomIO
  b <- randomIO
  return (r,g,b)

sinBrightness :: Frame -> Frame -> StateIO Double Frame
sinBrightness frame _ = do
  i <- get
  put (i + 0.01)
  let factor = 1 - abs (sin (i + 3.1415926535/2))
  return (flip setBrightness factor <$> frame)
