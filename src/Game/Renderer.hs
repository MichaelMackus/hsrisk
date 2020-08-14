{-# LANGUAGE OverloadedStrings #-}
module Game.Renderer (renderLoadingScreen, updateRenderer) where

import Game.Types

import Control.Monad.Reader
import Foreign.C.Types
import Graphics.Rect
import SDL
import qualified SDL.Font as Font
import qualified Control.Monad.State as State

bgColor = V4 200 200 200 200

renderLoadingScreen :: Renderer -> Font.Font -> IO ()
renderLoadingScreen r f = do
  clear r
  rendererDrawColor r $= bgColor
  s       <- Font.solid f (V4 0 0 0 255) "Loading..."
  fontT   <- createTextureFromSurface r s
  texInfo <- queryTexture fontT
  target  <- mkRectangleWithin (textureDimensions texInfo) (V2 1227 600)
  copy r fontT Nothing (Just target)
  present r

updateRenderer :: GameRenderer ()
updateRenderer = do
  {-- initialize renderer --}
  r       <- asks renderer
  liftIO $ clear r
  liftIO $ rendererDrawColor r $= bgColor
  maxSize <- get . windowSize =<< asks window
  texInfo <- queryTexture =<< asks texture
  tex     <- asks texture
  --target  <- mkRectangleWithin (textureDimensions texInfo) maxSize
  target  <- liftIO $ mkRectangleCroppedTo (textureDimensions texInfo) maxSize
  liftIO $ copy r tex Nothing (Just target)

  r   <- asks renderer
  i   <- asks index
  reg <- State.gets region
  case reg of
    Nothing  -> return ()
    Just reg -> do
      (rect, tex) <- getRegion reg
      liftIO $ copy r tex Nothing (Just rect)

getRegion :: Int -> GameRenderer (Rectangle CInt, Texture)
getRegion reg = do
    rs <- asks regions
    return (rs !! reg)
