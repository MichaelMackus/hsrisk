module Util where

import Codec.Picture
import Data.List.Split (chunksOf)
import GHC.Word (Word8)
import SDL

isTransparent :: PixelRGBA8 -> Bool
isTransparent = (==0) . pixelOpacity

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

grey :: PixelRGBA8
grey = PixelRGBA8 200 200 200 200

pixelToV4 :: PixelRGBA8 -> V4 Word8
pixelToV4 (PixelRGBA8 r g b a) = V4 r g b a

pixelToV3 :: PixelRGBA8 -> V3 Word8
pixelToV3 (PixelRGBA8 r g b a) = V3 r g b

pixelAlpha :: PixelRGBA8 -> Word8
pixelAlpha (PixelRGBA8 r g b a) = a

outOfBounds :: (Int, Int) -> Image a -> Bool
outOfBounds (x, y) i = x >= imageWidth  i || x < 0 || y >= imageHeight i || y < 0

fromXY :: Integral a => (Int, Int) -> Point V2 a
fromXY (x, y) = P (V2 (fromIntegral x) (fromIntegral y))

toXY :: Integral a => Point V2 a -> (Int, Int)
toXY (P (V2 x y)) = (fromIntegral x, fromIntegral y)

ptoi :: Integral a => Point V2 a -> Point V2 Int
ptoi (P (V2 x y)) = (P (V2 (fromIntegral x) (fromIntegral y)))

numerate :: [a] -> [(Int, a)]
numerate = zip [0..]

next :: Eq a => Maybe a -> [a] -> Maybe a
next _ []             = Nothing
next Nothing    (x:_) = Just x
next (Just e) l@(x:_) = Just $ case dropWhile (/= e) l of
                          (_:y:_) -> y
                          _       -> x

-- spit lists into a balanced list of lists num length
-- if list is not evenly divisible, the remaining elements will be
-- evenly distributed onto the front of the result list
splitBalanced :: Int -> [a] -> [[a]]
splitBalanced num xs 
        | num <= 0        = error "Must pass num > 0 to splitBalanced"
        | num > length xs = error "Must pass num <= length xs to splitBalanced" 
        | otherwise       = let chunkLen = floor (fromIntegral (length xs) / fromIntegral num)
                                chunks = chunksOf chunkLen xs
                                (balanced, rem) = (take num chunks, last chunks)
                            in  if length chunks == num then balanced
                                else distribute rem balanced []
    where distribute :: [a] -> [[a]] -> [[a]] -> [[a]]
          distribute []     ls     ls' = ls' ++ ls
          distribute (x:xs) (l:ls) ls' = distribute xs ls (ls' ++ [x:l])

