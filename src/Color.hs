{-# LANGUAGE FlexibleInstances #-}

module Color where

import System.Random
import Data.Word
import Control.Monad

type Color = (Word8, Word8, Word8)

instance Num (Word8, Word8, Word8) where
  (+) (r1,g1,b1) (r2,g2,b2) = (r1+r2,g1+g2,b1+b2)
  (*) (r1,g1,b1) (r2,g2,b2) = (r1*r2,g1*g2,b1*b2)
  abs (r,g,b) = (abs r, abs g, abs b)
  signum (r,g,b) = (signum r, signum g, signum b)
  fromInteger i = (fromInteger i, fromInteger i, fromInteger i)

setBrightness :: Color -> Double -> Color
setBrightness (r',g',b') fac =
  let (r,g,b) = (fromIntegral r', fromIntegral g', fromIntegral b') in
    (round $ r * fac, round $ g * fac, round $ b * fac)

fromRGB :: Word8 -> Word8 -> Word8 -> Color
fromRGB = (,,)

fromBGR :: Word8 -> Word8 -> Word8 -> Color
fromBGR b g r = fromRGB r g b

randomColors :: Int -> IO [Color]
randomColors amount = replicateM amount $ do
  r <- randomIO
  g <- randomIO
  b <- randomIO
  return (r `mod` 255, g `mod` 255, b `mod` 255)

black :: Color
black = (0, 0, 0)
