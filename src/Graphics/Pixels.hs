module Graphics.Pixels
    ( loadImage
     ,createSurfaceFromImage
     ,adjacentPixels
    ) where

import Codec.Picture
import SDL
import System.Endian
import qualified Data.IntSet as I
import qualified Data.Vector.Storable as V

loadImage :: String -> IO (Image PixelRGBA8)
loadImage f = do
    r <- readImage f
    case r of
        Left  e -> error e
        Right i -> do
            return (convertRGBA8 i)

createSurfaceFromImage :: Image PixelRGBA8 -> IO Surface
createSurfaceFromImage i = do
    pixels <- V.thaw (imageData i)
    let fmt = case getSystemEndianness of
                LittleEndian -> ABGR8888
                BigEndian    -> RGBA8888
    let (w, h) = (fromIntegral (imageWidth i), fromIntegral (imageHeight i))
    createRGBSurfaceFrom pixels (V2 w h) (fromIntegral w*4) fmt

-- gets the list of adjacent pixels of the same color
adjacentPixels :: (Int, Int) -> Image PixelRGBA8 -> [(Int, Int)]
adjacentPixels (x,y) i = adjacentToIntSet [(x,y)] mempty
    where
        adjacentToIntSet [] visited          = []
        adjacentToIntSet ((x,y):queue) visited
            | not (isPixel (x,y))            = adjacentToIntSet queue visited -- isPixel seems to be the next bottleneck
            | toInt (x,y) `I.member` visited = adjacentToIntSet queue visited
            | otherwise                      =
                let visited' = I.insert (toInt (x,y)) visited
                    queue'   = (x+1,y):(x-1,y):(x,y+1):(x,y-1):queue
                in  (x,y):adjacentToIntSet queue' visited'

        p = pixelAt i x y
        outOfBounds (x, y) = x >= imageWidth  i || x < 0 || y >= imageHeight i || y < 0
        toInt (x,y) = (imageWidth i)*y + x
        isPixel (x, y) = not (outOfBounds (x, y)) && pixelAt i x y == p

imageGrid :: Image PixelRGBA8 -> [(Int, Int)]
imageGrid i = let w = imageWidth i
                  h = imageHeight i
              in  [ (x, y) | x <- [0..w], y <- [0..h] ]
