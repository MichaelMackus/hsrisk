{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Rect
import Graphics.Pixels

import Codec.Picture
import Control.Monad (unless)
import SDL
import SDL.Image (load)
import SDL.Vect (V4(..))
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  initializeAll
  HintRenderScaleQuality $= ScaleLinear
  let windowConfig = defaultWindow { windowResizable = True }
  window   <- createWindow "My SDL Application" windowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  {-- load our image and obtain the raw pixel data --}
  image    <- loadImage "res/image/Risk_game_map_fixed.png"
  surface  <- createSurfaceFromImage image
  {-- create the SDL texture and pass to game loop --}
  texture  <- createTextureFromSurface renderer surface
  appLoop window renderer texture

bgColor = V4 200 200 200 200

appLoop :: Window -> Renderer -> Texture -> IO ()
appLoop window renderer texture = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          WindowClosedEvent _ -> True
          _ -> False
      qPressed = any eventIsQPress events
  clear renderer
  rendererDrawColor renderer $= bgColor
  maxSize <- get (windowSize window)
  texInfo <- queryTexture texture
  target  <- mkRectangleWithin (textureDimensions texInfo) maxSize
  copy renderer texture Nothing (Just target)
  present renderer
  unless qPressed (appLoop window renderer texture)
