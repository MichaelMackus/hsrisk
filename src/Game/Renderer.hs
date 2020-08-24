{-# LANGUAGE OverloadedStrings #-}
module Game.Renderer (renderLoadingScreen, updateRenderer, maxWidth, maxHeight) where

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

bgColor = pixelToV4 grey
maxWidth  = 1280
maxHeight = 720

renderLoadingScreen :: Window -> Renderer -> Font.Font -> IO ()
renderLoadingScreen window r font = f "Loading"
  where f text = do
          clear r
          rendererDrawColor r $= bgColor
          s       <- Font.solid font (V4 0 0 0 255) text
          fontT   <- createTextureFromSurface r s
          texInfo <- queryTexture fontT
          target  <- mkRectangleWithin (textureDimensions texInfo) (V2 maxWidth maxHeight)
          copy r fontT Nothing (Just target)
          present r
          threadDelay 300000
          unless (text == "Loading...") $ f (T.append text ".")

updateRenderer :: GameRenderer ()
updateRenderer = do
  {-- initialize renderer --}
  r       <- asks renderer
  f       <- asks font
  fLineH  <- Font.lineSkip f
  bgTex   <- asks background
  bgInfo  <- queryTexture bgTex

  {-- draw BG image --}
  liftIO $ clear r
  liftIO $ rendererDrawColor r $= bgColor
  liftIO $ copy r bgTex Nothing (Just (textureRect bgInfo))

  {-- draw messages --}
  msgs <- State.gets messages
  let (V2 bgW bgH)   = textureDimensions bgInfo
      maxMsgs        = floor (fromIntegral (maxHeight - bgH) / fromIntegral fLineH)
  forM_ (take maxMsgs (numerate msgs)) $ \(i, msg) -> do
      s        <- Font.blended f (V4 0 0 0 255) (T.pack msg)
      fontT    <- createTextureFromSurface r s
      fontInfo <- queryTexture fontT
      let (V2 w h) = textureDimensions fontInfo
          padding  = 10
          offset   = fromIntegral (fLineH * i)
          fontRect = Rectangle (P (V2 padding (bgH + offset))) (V2 w h)
      liftIO $ copy r fontT Nothing (Just fontRect)
      freeSurface s
      destroyTexture fontT

  {-- draw occupied player & army count --}
  ts      <- M.toList <$> State.gets occupiedTerritories
  liftIO $ Font.setStyle f [Font.Bold]
  forM ts $ \(t, (p, armyCnt)) -> do 
      {-- draw color of occupying player --}
      tex  <- territoryTex t
      rect <- territoryRect t
      let color = playerColor p
      textureAlphaMod tex $= pixelAlpha color
      textureColorMod tex $= pixelToV3 color
      liftIO $ copy r tex Nothing (Just rect)
      {-- draw occupied army count --}
      idx      <- asks index
      s        <- Font.blended f (V4 0 0 0 255) (T.pack (show armyCnt))
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
  liftIO $ Font.setStyle f []

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
      textureAlphaMod tex $= 125
      textureColorMod tex $= V3 255 255 255
      liftIO $ copy r tex Nothing (Just rect)
      -- highlight territory connections (TODO another color)
      conns <- connectedTo t
      forM_ conns $ \t -> do
          tex  <- territoryTex t
          rect <- territoryRect t
          textureAlphaMod tex $= 125
          textureColorMod tex $= V3 200 200 200
          liftIO $ copy r tex Nothing (Just rect)
