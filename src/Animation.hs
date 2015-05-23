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
data Animation m = Animation { step :: Frame -> m (Maybe Frame), startFrame :: Frame }

type StateIO a b = StateT a IO b

runAnimation :: MonadIO m => SerialPort -> Int -> Animation m -> m ()
runAnimation s delay animation = runAnim (Just $ startFrame animation)
  where
    runAnim mode = flip (maybe (return ())) mode $ \frame -> do
      liftIO $ do
        P.perform s (P.fill frame)
        threadDelay delay
      runAnim =<< step animation frame

fill :: Monad m => Frame -> Animation m
fill frame = Animation (return . return) frame

cycleLeft :: MonadIO m => Frame -> m (Maybe Frame)
cycleLeft frame = liftIO $ return $ Just (drop (length frame - 1) frame ++ take (length frame - 1) frame)

cycleRight :: MonadIO m => Frame -> m (Maybe Frame)
cycleRight frame = liftIO $ return $ Just (drop 1 frame ++ take 1 frame)

fillRandom :: MonadIO m => m (Maybe Frame)
fillRandom = fmap Just $ liftIO $ replicateM 30 $ do
  r <- randomIO
  g <- randomIO
  b <- randomIO
  return (r,g,b)

sinBrightness :: Frame -> Frame -> StateIO Double (Maybe Frame)
sinBrightness frame _ = do
  i <- get
  put (i + 0.01)
  let factor = 1 - abs (sin (i + 3.1415926535/2))
  return $ if i >= 3.141592 then
      Nothing
    else
      Just (flip setBrightness factor <$> frame)
