module Graphics.Image
    ( loadImage
     ,createSurfaceFromImage
     ,createSurfacesFromIndex
     ,createTexturesFromIndex
     ,regionRects
     ,ptoi
     ,fromXY
     ,toXY
     ,module Graphics.Image.Index
    ) where

import Graphics.Image.Index

import Codec.Picture
import Control.Monad (forM, when)
import Foreign.C.Types (CInt)
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
    let (w, h) = (fromIntegral (imageWidth i), fromIntegral (imageHeight i))
    createRGBSurfaceFrom pixels (V2 w h) (fromIntegral w*4) systemFormat

createSurfacesFromIndex :: IndexedImage -> IO [Surface]
createSurfacesFromIndex index = mapM (createSurfaceFromImage . snd) (indexRegions index)

createTexturesFromIndex :: Renderer -> IndexedImage -> IO [Texture]
createTexturesFromIndex renderer index = do
        surfaces <- createSurfacesFromIndex index
        mapM (createTextureFromSurface renderer) surfaces

regionRects :: Integral a => IndexedImage -> [Rectangle a]
regionRects index = map f (indexRegions index)
    where f ((x,y), i) = let (w,h) = (fromIntegral (imageWidth i), fromIntegral (imageHeight i))
                         in  Rectangle (fromXY (x,y)) (V2 w h)

fromXY :: Integral a => (Int, Int) -> Point V2 a
fromXY (x, y) = P (V2 (fromIntegral x) (fromIntegral y))

toXY :: Integral a => Point V2 a -> (Int, Int)
toXY (P (V2 x y)) = (fromIntegral x, fromIntegral y)

ptoi :: Integral a => Point V2 a -> Point V2 Int
ptoi (P (V2 x y)) = (P (V2 (fromIntegral x) (fromIntegral y)))

systemFormat :: PixelFormat
systemFormat = case getSystemEndianness of
                    LittleEndian -> ABGR8888
                    BigEndian    -> RGBA8888
