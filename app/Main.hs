{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Rect
import Graphics.Pixels

import Codec.Picture
import Control.Monad (unless)
import GHC.Int
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
  appLoop window renderer texture image

bgColor = V4 200 200 200 200

appLoop :: Window -> Renderer -> Texture -> Image PixelRGBA8 -> IO ()
appLoop window renderer texture image = do
  -- handle events
  events <- map (handleEvent . eventPayload) <$> pollEvents
  let quit = any (== QuitGame) events
  -- render
  clear renderer
  rendererDrawColor renderer $= bgColor
  maxSize <- get (windowSize window)
  texInfo <- queryTexture texture
  target  <- mkRectangleWithin (textureDimensions texInfo) maxSize
  copy renderer texture Nothing (Just target)
  present renderer
  unless quit (appLoop window renderer texture image)

data GameEvent = QuitGame | MouseHover (Point V2 Int32) | MouseClick MouseButton (Point V2 Int32) | OtherEvent deriving Eq

handleEvent :: EventPayload -> GameEvent
handleEvent (MouseButtonEvent  e)
    | mouseButtonEventButton e == ButtonLeft = MouseClick ButtonLeft (mouseButtonEventPos e)
handleEvent (KeyboardEvent e)
    | keyboardEventKeyMotion e == Pressed && keysymKeycode (keyboardEventKeysym e) == KeycodeQ = QuitGame
handleEvent (WindowClosedEvent _) = QuitGame
handleEvent otherwise = OtherEvent
