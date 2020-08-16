{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game
import Graphics.Image
import Graphics.Image.Util

import Codec.Picture
import Data.Maybe
import SDL

-- saves an image's regions completely in their color (overwriting contents)
main :: IO ()
main = saveLayer True "res/image/risk-map-numbers.png" >> saveLayer False "res/image/risk-map-annotations.png"

saveLayer :: Bool -> String -> IO ()
saveLayer numbers fname = do
      {-- initialize SDL --}
      initializeAll
      HintRenderScaleQuality $= ScaleLinear
      -- let windowConfig = defaultWindow { windowResizable = True, windowWidth = 1227, windowHeight = 628 }
      let windowConfig = defaultWindow { windowResizable = False, windowInitialSize = V2 1227 628 }
      image  <- loadImage "res/image/Risk_game_map_fixed.png"
      --image'  <- loadImage "res/image/risk-map-no-borders.png"
      image' <- loadImage "res/image/risk-map-no-black.png"
      putStrLn "Image loaded"
      let index   = indexImage image
          image'' = generateImage (f image image') (imageWidth image) (imageHeight image)
      savePngImage fname $ ImageRGBA8 image''
      putStrLn "Image saved!"
  where
      f i i' x y = let p  = pixelAt i  x y
                       p' = pixelAt i' x y
                       r | p == black      = if isTransparent p' then if numbers then clear else p
                                             else if numbers then p else clear
                         | otherwise       = clear
                   in  r
      clear = PixelRGBA8 0 0 0 0

saveWithoutBlack :: IO ()
saveWithoutBlack = do
      {-- initialize SDL --}
      initializeAll
      HintRenderScaleQuality $= ScaleLinear
      -- let windowConfig = defaultWindow { windowResizable = True, windowWidth = 1227, windowHeight = 628 }
      let windowConfig = defaultWindow { windowResizable = False, windowInitialSize = V2 1227 628 }
      image <- loadImage "res/image/risk-map-no-borders.png"
      seq image $ putStrLn "Image loaded"
      let index  = indexImage image
          image' = generateImageFromIndex index
      savePngImage "res/image/risk-map-no-black.png" $ ImageRGBA8 image'
      putStrLn "Image saved!"
  where
      generateImageFromIndex :: IndexedImage -> Image PixelRGBA8
      generateImageFromIndex index = generateImage f (imageWidth i) (imageHeight i)
          where f x y   = let r = findPixel index (x, y)
                          in case r of
                              Just  r -> let (x', y') = xys !! r
                                         in  pixelAt i x' y'
                              Nothing -> PixelRGBA8 0 0 0 0
                xys     = map fst (indexRegions index)
                i       = image index

      indexFilter :: PixelRGBA8 -> Bool
      indexFilter p = not (isTransparent p || isBlack p)
          where
            isTransparent :: PixelRGBA8 -> Bool
            isTransparent = (==0) . pixelOpacity
            isBlack :: PixelRGBA8 -> Bool
            isBlack (PixelRGBA8 0 0 0 255) = True
            isBlack otherwise = False
