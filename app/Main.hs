{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game
import Graphics.Image

import SDL

main :: IO ()
main = do
  {-- load background image index --}
  image <- loadImage "res/image/Risk_game_map_fixed.png"
  seq image $ putStrLn "Image loaded"
  {-- initialize SDL --}
  initializeAll
  HintRenderScaleQuality $= ScaleLinear
  -- let windowConfig = defaultWindow { windowResizable = True, windowWidth = 1227, windowHeight = 628 }
  let windowConfig = defaultWindow { windowResizable = False, windowInitialSize = V2 1227 628 }
  window   <- createWindow "My SDL Application" windowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  {-- pass off to game loop --}
  runGame window renderer image
