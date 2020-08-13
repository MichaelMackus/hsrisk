module Graphics.Rect
    ( mkRectangleWithin
     ,mkRectangleCroppedTo
     ,textureDimensions
    ) where

import SDL
import SDL.Vect (V4(..), V2(..), Point(..))
import Foreign.C.Types

mkRectangleWithin :: V2 CInt -> V2 CInt -> IO (Rectangle CInt)
mkRectangleWithin (V2 w h) (V2 outerw outerh) =
    let (w', h') = clampDimensionsTo (w, h) (outerw, outerh)
        (x,  y ) = translateOrigin (w, h) (outerw, outerh)
    in  return $ Rectangle (P (V2 x y)) (V2 w' h')

mkRectangleCroppedTo :: V2 CInt -> V2 CInt -> IO (Rectangle CInt)
mkRectangleCroppedTo (V2 w h) (V2 outerw outerh) =
    let (w', h') = clampDimensionsTo (w, h) (outerw, outerh)
    in  return $ Rectangle (P (V2 0 0)) (V2 w' h')

textureDimensions :: TextureInfo -> V2 CInt
textureDimensions (TextureInfo _ _ w h) = V2 w h

-- translate origin so that the rect is in the center of outerw x outerh
translateOrigin :: Integral a => (a, a) -> (a, a) -> (a, a)
translateOrigin (w, h) (outerw, outerh) = (centerx, centery)
    where
        centerx = round $ max 0 ((fromIntegral outerw - fromIntegral w) / 2)
        centery = round $ max 0 ((fromIntegral outerh - fromIntegral h) / 2)

-- ensure dimensions are clamped to maxw x maxh
clampDimensionsTo :: Integral a => (a, a) -> (a, a) -> (a, a)
clampDimensionsTo (w, h) (maxw, maxh)
        | w < maxw && h < maxh = (w, h)
        | w > h     = (maxw, round ((fromIntegral maxw)/aspect))
        | otherwise = (round ((fromIntegral maxh)*aspect), maxh)
    where
        aspect = fromIntegral w / fromIntegral h
