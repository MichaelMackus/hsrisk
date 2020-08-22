{-# LANGUAGE OverloadedStrings #-}
module Game.Renderer (renderLoadingScreen, updateRenderer) where

import Game.Types
import Graphics.Image (regionRect)
import Graphics.Image.Index (IndexedImage(..))
import Util

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Foreign.C.Types
import Graphics.Rect
import SDL
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
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

  {-- draw occupied army & count --}
  ts      <- M.toList <$> State.gets occupiedTerritories
  forM ts $ \(t, (p, armyCnt)) -> do 
      idx      <- asks index
      f        <- asks font
      s        <- Font.blended f (V4 0 0 0 255) (T.pack (show p ++ " - " ++ show armyCnt))
      fontT    <- createTextureFromSurface r s
      texInfo  <- queryTexture fontT
      terrRect <- territoryRect t
      let (Rectangle (P (V2 sx sy)) (V2 rw rh)) = terrRect
          (V2 tw th) = textureDimensions texInfo
          (x, y)     = (sx + floor (fromIntegral rw / 2), sy + floor (fromIntegral rh / 2))
      -- target   <- liftIO $ mkRectangleWithin (textureDimensions texInfo) tRect
      liftIO $ copy r fontT Nothing $ Just (Rectangle (P (V2 x y)) (V2 tw th))
      freeSurface s
      destroyTexture fontT

  {-- draw highlighted territory --}
  r   <- asks renderer
  i   <- asks index
  t   <- State.gets hovering
  case t of
    Nothing  -> return ()
    Just t   -> do
      -- highlight territory region
      tex  <- territoryTex t
      rect <- territoryRect t
      textureColorMod tex $= V3 255 255 255
      liftIO $ copy r tex Nothing (Just rect)
      -- highlight territory connections (TODO another color)
      conns <- connectedTo t
      forM_ conns $ \t -> do
          tex  <- territoryTex t
          rect <- territoryRect t
          textureColorMod tex $= V3 200 200 200
          liftIO $ copy r tex Nothing (Just rect)
