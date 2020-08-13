{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Rect
import Graphics.Image
import Graphics.Image.Index

import Codec.Picture
import Codec.Picture.Types (pixelMapXY)
import Control.Monad (unless, forM_)
import Data.Maybe (isJust)
import Foreign.C.Types
import GHC.Int
import SDL
import SDL.Image (load)
import SDL.Vect (V4(..))
import System.IO
import qualified Data.IntSet as I
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
      {-- load background image index --}
      image <- loadImage "res/image/Risk_game_map_fixed.png"
      seq image $ putStrLn "Image loaded"
      let index = indexImage image
          -- image' = highlightRegions index image
      seq index $ putStrLn "Image indexed"
      {-- initialize SDL --}
      initializeAll
      HintRenderScaleQuality $= ScaleLinear
      let windowConfig = defaultWindow { windowResizable = True }
      window   <- createWindow "My SDL Application" windowConfig
      renderer <- createRenderer window (-1) defaultRenderer
      {-- load our image and obtain the raw pixel data --}
      -- seq image' $ putStrLn "Regions highlighted"
      surface  <- createSurfaceFromImage image
      {-- create the SDL texture and pass to game loop --}
      texture  <- createTextureFromSurface renderer surface
      appLoop window renderer texture index
    where
      highlightRegions :: IndexedImage -> Image PixelRGBA8 -> Image PixelRGBA8
      highlightRegions index i = pixelMapXY f i
          where f x y p = case findPixel index (x,y) of
                            Just i  -> PixelRGBA8 255 (fromIntegral $ i*2) (fromIntegral $ i*2) 255
                            Nothing -> p

bgColor = V4 200 200 200 200

appLoop :: Window -> Renderer -> Texture -> IndexedImage -> IO ()
appLoop window renderer texture index = do
  -- handle events
  events  <- map (toGameEvent . eventPayload) <$> pollEvents
  forM_ events (handleEvent renderer texture index)
  -- render
  clear renderer
  rendererDrawColor renderer $= bgColor
  maxSize <- get (windowSize window)
  texInfo <- queryTexture texture
  --target  <- mkRectangleWithin (textureDimensions texInfo) maxSize
  target  <- mkRectangleCroppedTo (textureDimensions texInfo) maxSize
  copy renderer texture Nothing (Just target)
  present renderer
  unless (any (== QuitGame) events) (appLoop window renderer texture index)

data GameEvent = QuitGame | MouseHover (Point V2 Int) | MouseClick MouseButton (Point V2 Int) | OtherEvent deriving Eq

handleEvent :: Renderer -> Texture -> IndexedImage -> GameEvent -> IO ()
handleEvent r t i (MouseHover (P (V2 x y))) = do
        putStrLn $ "Mouse moving: " ++ show x ++ ", " ++ show y
        case findPixel i (x,y) of
            Just r  -> putStrLn $ "Within region: " ++ show r
            Nothing -> putStrLn "No region"
handleEvent r t i otherwise = return ()

toGameEvent :: EventPayload -> GameEvent
toGameEvent (MouseButtonEvent  e)
    | mouseButtonEventButton e == ButtonLeft = MouseClick ButtonLeft (ptoi $ mouseButtonEventPos e)
toGameEvent (MouseMotionEvent  e) = MouseHover (ptoi $ mouseMotionEventPos e)
toGameEvent (KeyboardEvent e)
    | keyboardEventKeyMotion e == Pressed && keysymKeycode (keyboardEventKeysym e) == KeycodeQ = QuitGame
toGameEvent (WindowClosedEvent _) = QuitGame
toGameEvent otherwise = OtherEvent

ptoi :: Integral a => Point V2 a -> Point V2 Int
ptoi (P (V2 x y)) = (P (V2 (fromIntegral x) (fromIntegral y)))
