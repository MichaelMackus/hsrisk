{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game
import Game.Renderer (maxWidth, maxHeight)
import Graphics.Image

import SDL
import System.Environment (getArgs)

main :: IO ()
main = do
  {-- initialize SDL --}
  initialize [InitVideo, InitEvents]
  HintRenderScaleQuality $= ScaleLinear
  isFullscreen <- any (`elem` ["--fullscreen", "-f"]) <$> getArgs
  let windowConfig   = defaultWindow   { windowResizable = False,
                                         windowInitialSize = V2 maxWidth maxHeight,
                                         windowMode = if isFullscreen then Fullscreen else Windowed }
      rendererConfig = defaultRenderer { rendererType = AcceleratedVSyncRenderer }
  window   <- createWindow "My SDL Application" windowConfig
  renderer <- createRenderer window (-1) rendererConfig
  rendererLogicalSize renderer $= Just (V2 maxWidth maxHeight)
  {-- pass off to game loop --}
  runGame window renderer
