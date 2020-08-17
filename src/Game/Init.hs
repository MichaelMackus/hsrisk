module Game.Init where

import Game.Types
import Game.Renderer
import Graphics.Image
import Graphics.Image.Util

import Codec.Picture (Image(..), PixelRGBA8(..), generateImage, pixelAt)
import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe (isJust)
import SDL
import qualified SDL.Font as Font

initGame :: Window -> Renderer -> IO (Maybe RendererEnv)
initGame window renderer = do
    Font.initialize
    font   <- Font.load "res/font/LiberationSans-Regular.ttf" 16
    {-- load game assets in separate thread --}
    shared <- atomically $ newTVar Nothing
    forkIO $ do
        {-- load background image --}
        image <- loadImage "res/image/risk-map-continent-colors.png"
        putStrLn "Image loaded"
        {-- convert image to SDL --}
        surface  <- createSurfaceFromImage image
        texture  <- createTextureFromSurface renderer surface
        {-- load image without numbers or borders to index regions --}
        image' <- loadImage "res/image/risk-map-connected-regions.png"
        putStrLn "Index image loaded"
        let index   = indexImage image'
            {-- the following is necessary so we mask out colors not in original map --}
            images' = map (initRegionImage image index) [0..length (indexRegions index) - 1] 
        -- TODO initialize image regions to default color
        when (length (indexRegions index) /= 42) (error "Unable to continue - territories in index does not equal 42!")
        putStrLn "Image indexed"
        when (length (indexRegions index) /= length images') (error "Unable to continue - territories do not equal mask images!")
        putStrLn "Index masked"
        surfaces <- mapM createSurfaceFromImage images'
        textures <- mapM (createTextureFromSurface renderer) surfaces
        putStrLn "Region textures loaded"
        let territories = initTerritories index image textures
            env         = RendererEnv window renderer texture font index territories
        atomically $ writeTVar shared (Just env)
    {-- wait to play game until assets are loaded --}
    waitUntilLoaded renderer font shared
    atomically $ readTVar shared

waitUntilLoaded :: Renderer -> Font.Font -> TVar (Maybe RendererEnv) -> IO ()
waitUntilLoaded r f shared = do
    index <- atomically $ readTVar shared
    if isJust index then return ()
    else do
        renderLoadingScreen r f
        waitUntilLoaded r f shared

initTerritories :: IndexedImage -> Image PixelRGBA8 -> [Texture] -> [Territory]
initTerritories _     _     []       = []
initTerritories index bgimg textures = map initTerritory [0..length textures - 1]
    where rects = regionRects index
          initTerritory r = Territory cont conns (rects !! r, textures !! r) numberLoc
              where cont      = Continent NAmerica (fromXY (0,0))
                    conns     = []
                    region    = colorRegions index !! r
                    (x,y)     = (regionStartX region, regionStartY region)
                    (w,h)     = (regionWidth  region, regionHeight region)
                    numberLoc = fromXY (x + floor ((fromIntegral w)/2), y + floor ((fromIntegral h)/2))

initRegionImage :: Image PixelRGBA8 -> IndexedImage -> Int -> Image PixelRGBA8
initRegionImage bgimg index r = let ((sx,sy),i) = indexRegions index !! r
                                    (w,h) = (imageWidth i, imageHeight i)
                                in  generateImage (g r (sx,sy)) w h :: Image PixelRGBA8
    where g r (sx,sy) x y = let pi        = pixelAt bgimg (sx + x) (sy + y)
                                r'        = findPixel index (sx + x, sy + y)
                                inCountry = not (isTransparent pi || pi == black)
                            in  case r' of
                                    (Just r') | r' == r && inCountry
                                                           -> PixelRGBA8 255 255 255 125
                                              | otherwise  -> PixelRGBA8 0   0   0   0
                                    Nothing                -> PixelRGBA8 0   0   0   0
