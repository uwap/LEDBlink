{-# LANGUAGE OverloadedStrings #-}
module Websocket where

import Control.Concurrent
import Control.Monad
import Data.Text (Text, unpack)
import Data.Maybe
import Text.Read

import qualified Network.WebSockets as WS
import Animation
import Color

start :: MVar Animation -> Counter -> IO ()
start animation counter = WS.runServer "127.0.0.1" 8080 $ webSocketServer animation counter

webSocketServer :: MVar Animation -> Counter -> WS.PendingConnection -> IO ()
webSocketServer animation counter req = do
  conn <- WS.acceptRequest req
  forever $ do
    dat <- WS.receiveData conn :: IO Text
    putMVar animation =<< case dat of
      "randomCycleRight" -> return $ fillRandom >=> cycleRight 
      "randomSin"        -> return $ fillRandom >=> sinBrightness counter
      "fillColor"        -> return . fromMaybeColor fillColor =<< readColor conn
      "colorSin"         -> (\color -> return (fromMaybeColor fillColor color >=> sinBrightness counter)) =<< readColor conn
      _                  -> return $ fillColor $ Color 0 255 255 255


readColor :: WS.Connection -> IO (Maybe Color)
readColor conn = do
  red   <- readMaybe . unpack <$> WS.receiveData conn
  green <- readMaybe . unpack <$> WS.receiveData conn
  blue  <- readMaybe . unpack <$> WS.receiveData conn
  return (Color 1 <$> red <*> green <*> blue)

defaultOr :: Maybe Color -> Color
defaultOr = fromMaybe (Color 0 255 255 255)

fromMaybeColor :: (Color -> Animation) -> Maybe Color -> Animation
fromMaybeColor f c = f $ defaultOr c
