{-# LANGUAGE OverloadedStrings #-}
module Game.Renderer (renderLoadingScreen, updateRenderer) where

import Game.Types
import Graphics.Image (regionRect)
import Graphics.Image.Index (IndexedImage(..))
import Graphics.Image.Util

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Foreign.C.Types
import Graphics.Rect
import SDL
import qualified Data.Text as T
import qualified Data.List as L
import qualified SDL.Font as Font
import qualified Control.Monad.State as State

bgColor = pixelToSDL grey

renderLoadingScreen :: Renderer -> Font.Font -> IO ()
renderLoadingScreen r font = f "Loading"
  where f text = do
          clear r
          rendererDrawColor r $= bgColor
          s       <- Font.solid font (V4 0 0 0 255) text
          fontT   <- createTextureFromSurface r s
          texInfo <- queryTexture fontT
          target  <- mkRectangleWithin (textureDimensions texInfo) (V2 1227 600)
          copy r fontT Nothing (Just target)
          present r
          threadDelay 300000
          unless (text == "Loading...") $ f (T.append text ".")

updateRenderer :: GameRenderer ()
updateRenderer = do
  {-- initialize renderer --}
  r       <- asks renderer
  maxSize <- get . windowSize =<< asks window
  texture <- asks background
  texInfo <- queryTexture texture
  target  <- liftIO $ mkRectangleCroppedTo (textureDimensions texInfo) maxSize

  {-- draw BG image --}
  liftIO $ clear r
  liftIO $ rendererDrawColor r $= bgColor
  liftIO $ copy r texture Nothing (Just target)

  {-- draw font (WIP) --}
  ts      <- asks territories
  forM ts $ \t -> do 
      let (Just i) = L.elemIndex t ts
      idx      <- asks index
      f        <- asks font
      s        <- Font.blended f (V4 0 0 0 255) (T.pack (show i))
      fontT    <- createTextureFromSurface r s
      texInfo  <- queryTexture fontT
      let (Rectangle (P (V2 sx sy)) (V2 rw rh)) = uncurry regionRect (indexRegions idx !! i)
          (V2 tw th) = textureDimensions texInfo
          (x, y)     = (sx + floor (fromIntegral rw / 2), sy + floor (fromIntegral rh / 2))
      -- target   <- liftIO $ mkRectangleWithin (textureDimensions texInfo) tRect
      liftIO $ copy r fontT Nothing $ Just (Rectangle (P (V2 x y)) (V2 tw th))
      freeSurface s
      destroyTexture fontT

  {-- draw highlighted territory --}
  r   <- asks renderer
  i   <- asks index
  reg <- State.gets region
  case reg of
    Nothing  -> return ()
    Just reg -> do
      (rect, tex) <- getTerritory reg
      liftIO $ copy r tex Nothing (Just rect)

getTerritory :: Int -> GameRenderer (Rectangle CInt, Texture)
getTerritory r = do
    ts <- asks territories
    let t = ts !! r
    return (tRenderData t)
