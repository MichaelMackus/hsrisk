module Graphics.Pixels
    ( loadImage
     ,createSurfaceFromImage
    ) where

import Codec.Picture
import SDL
import System.Endian
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
