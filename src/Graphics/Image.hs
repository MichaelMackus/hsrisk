module Graphics.Image
    ( loadImage
     ,createSurfaceFromImage
     ,createSurfacesFromIndex
     ,createTexturesFromIndex
     ,ptoi
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

createSurfacesFromIndex :: IndexedImage -> IO [(Rectangle CInt, Surface)]
createSurfacesFromIndex index = let len = length (colorRegions index) 
                                in  if len == 0 then error "Index has no regions!"
                                    else mapM f [0..len - 1]
    where f i = regionToSurface (imageWidth (image index)) (colorSets index !! i) (colorRegions index !! i)

createTexturesFromIndex :: Renderer -> IndexedImage -> IO [(Rectangle CInt, Texture)]
createTexturesFromIndex renderer index = do
        r <- createSurfacesFromIndex index
        forM r $ \(rect, s) -> do
            t <- createTextureFromSurface renderer s
            return (rect, t)

regionToSurface :: Int -> I.IntSet -> [(Int, Int)] -> IO (Rectangle CInt, Surface)
regionToSurface imageW i r = do
    let (w,h)   = (regionWidth  r, regionHeight r)
        (sx,sy) = (regionStartX r, regionStartY r)
        image   = generateImage f w h
        f x y   = if pToInt imageW (sx + x, sy + y) `I.member` i then PixelRGBA8 255 255 255 125
                  else PixelRGBA8 0 0 0 0
    when (w == 0 || h == 0) $ error "Width/height must not be zero"
    surface <- createSurfaceFromImage image
    return (regionRect r, surface)

regionRect :: Integral a => [(Int, Int)] -> Rectangle a
regionRect r =
    let (x,y)   = (regionStartX r, regionStartY r)
        (w,h)   = (regionWidth  r, regionHeight r)
        start   = fromXY (x,y)
        size    = V2 (fromIntegral w) (fromIntegral h)
    in  Rectangle start size

fromXY :: Integral a => (Int, Int) -> Point V2 a
fromXY (x, y) = P (V2 (fromIntegral x) (fromIntegral y))

ptoi :: Integral a => Point V2 a -> Point V2 Int
ptoi (P (V2 x y)) = (P (V2 (fromIntegral x) (fromIntegral y)))

systemFormat :: PixelFormat
systemFormat = case getSystemEndianness of
                    LittleEndian -> ABGR8888
                    BigEndian    -> RGBA8888
