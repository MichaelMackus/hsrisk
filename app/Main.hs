{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game
import Graphics.Image

import SDL

main :: IO ()
main = do
  {-- initialize SDL --}
  initializeAll
  HintRenderScaleQuality $= ScaleLinear
  let windowConfig = defaultWindow { windowResizable = False, windowInitialSize = V2 1227 720 }
  window   <- createWindow "My SDL Application" windowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  {-- pass off to game loop --}
  runGame window renderer
