module Graphics.Image
    ( loadImage
     ,createSurfaceFromImage
     ,createSurfacesFromIndex
     ,createTexturesFromIndex
     ,regionRects
     ,regionRect
     ,module Graphics.Image.Index
    ) where

import Graphics.Image.Index
import Graphics.Image.Util

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
regionRects index = map (uncurry regionRect) (indexRegions index)

regionRect :: Integral a => (Int, Int) -> Image PixelRGBA8 -> Rectangle a
regionRect (x,y) i = let (w,h) = (fromIntegral (imageWidth i), fromIntegral (imageHeight i))
                     in  Rectangle (fromXY (x,y)) (V2 w h)


systemFormat :: PixelFormat
systemFormat = case getSystemEndianness of
                    LittleEndian -> ABGR8888
                    BigEndian    -> RGBA8888
