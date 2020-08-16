module Graphics.Image.Util where

import Codec.Picture
import GHC.Word (Word8)
import SDL

isTransparent :: PixelRGBA8 -> Bool
isTransparent = (==0) . pixelOpacity

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

grey :: PixelRGBA8
grey = PixelRGBA8 200 200 200 200

pixelToSDL :: PixelRGBA8 -> V4 Word8
pixelToSDL (PixelRGBA8 r g b a) = V4 r g b a

outOfBounds :: (Int, Int) -> Image a -> Bool
outOfBounds (x, y) i = x >= imageWidth  i || x < 0 || y >= imageHeight i || y < 0

fromXY :: Integral a => (Int, Int) -> Point V2 a
fromXY (x, y) = P (V2 (fromIntegral x) (fromIntegral y))

toXY :: Integral a => Point V2 a -> (Int, Int)
toXY (P (V2 x y)) = (fromIntegral x, fromIntegral y)

ptoi :: Integral a => Point V2 a -> Point V2 Int
ptoi (P (V2 x y)) = (P (V2 (fromIntegral x) (fromIntegral y)))

