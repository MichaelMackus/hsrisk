{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Rect
import Graphics.Image

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
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL

main :: IO ()
main = do
  {-- load background image index --}
  image <- loadImage "res/image/Risk_game_map_fixed.png"
  seq image $ putStrLn "Image loaded"
  let index = indexImage image
  seq index $ putStrLn "Image indexed"
  {-- initialize SDL --}
  initializeAll
  HintRenderScaleQuality $= ScaleLinear
  -- let windowConfig = defaultWindow { windowResizable = True, windowWidth = 1227, windowHeight = 628 }
  let windowConfig = defaultWindow { windowResizable = False, windowInitialSize = V2 1227 628 }
  window   <- createWindow "My SDL Application" windowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  {-- load our image and obtain the raw pixel data --}
  surface  <- createSurfaceFromImage image
  texture  <- createTextureFromSurface renderer surface
  regions  <- createTexturesFromIndex renderer index
  {-- pass off to game loop --}
  appLoop window renderer texture index regions

bgColor = V4 200 200 200 200

appLoop :: Window -> Renderer -> Texture -> IndexedImage -> [(Rectangle CInt, Texture)] -> IO ()
appLoop window renderer texture index regions = do
  {-- setup map in renderer --}
  clear renderer
  rendererDrawColor renderer $= bgColor
  maxSize <- get (windowSize window)
  texInfo <- queryTexture texture
  --target  <- mkRectangleWithin (textureDimensions texInfo) maxSize
  target  <- mkRectangleCroppedTo (textureDimensions texInfo) maxSize
  copy renderer texture Nothing (Just target)
  {-- handle events --}
  events  <- map (toGameEvent . eventPayload) <$> pollEvents
  forM_ events (handleEvent renderer index regions)
  -- TODO store previous hovered state & display region for it
  {-- render to screen --}
  present renderer
  unless (any (== QuitGame) events) (appLoop window renderer texture index regions)

data GameEvent = QuitGame | MouseHover (Point V2 Int) | MouseClick MouseButton (Point V2 Int) | OtherEvent deriving Eq

handleEvent :: Renderer -> IndexedImage -> [(Rectangle CInt, Texture)] -> GameEvent -> IO ()
handleEvent renderer index regions (MouseHover (P (V2 x y))) = do
        putStrLn $ "Mouse moving: " ++ show x ++ ", " ++ show y
        case findPixel index (x,y) of
            Just r  -> do
                putStrLn $ "Within region: " ++ show r
                let (rect, texture) = regions !! r
                copy renderer texture Nothing (Just rect)
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
