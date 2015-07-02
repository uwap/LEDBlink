{-# LANGUAGE TemplateHaskell #-}

module Color where

import Data.Word
import Data.Binary
import Control.Lens

data Color = Color { _brightness :: Double
                   , _r          :: Word8
                   , _g          :: Word8
                   , _b          :: Word8
                   }
makeLenses ''Color

instance Binary Color where
  put = put . toRGB
  get = uncurry2 fromRGB <$> get
    where
      uncurry2 f (x,y,z) = f x y z

setBrightness :: Double -> Color -> Color
setBrightness br color = color & brightness .~ br

fromRGB :: Word8 -> Word8 -> Word8 -> Color
fromRGB = Color 1

fromBGR :: Word8 -> Word8 -> Word8 -> Color
fromBGR blue green red = fromRGB red green blue

toRGB :: Color -> (Word8, Word8, Word8)
toRGB (Color br red green blue) = (applyBrightness red, applyBrightness green, applyBrightness blue)
                  where
                    applyBrightness :: Word8 -> Word8
                    applyBrightness c = round (br * fromIntegral c)
