{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Image (loadTexture)
import SDL.Vect (V4(..))
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  tex <- loadTexture renderer "res/image/Risk_game_map_fixed.png"
  appLoop renderer tex

appLoop :: Renderer -> Texture -> IO ()
appLoop renderer tex = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  clear renderer
  copy renderer tex Nothing Nothing
  present renderer
  unless qPressed (appLoop renderer tex)
