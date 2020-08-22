module Graphics.Image.Index
    (IndexedImage(..)
     ,indexImage
     ,filterIndex
     ,findPixel
     ,regionStartX
     ,regionStartY
     ,regionHeight
     ,regionWidth
     ,regionColor
    ) where

import Util

import Codec.Picture
import qualified Control.Monad.State as S
import qualified Data.IntSet as I
import qualified Data.List as L

-- TODO more efficient way to load indexed png
-- loadIndexedImage f = do
--     (Right i) <- readImage f
--     let irgb = convertRGB8 i
--     let (ip, p) = palettize (PaletteOptions Uniform False 256) irgb
--     return ip

-- this is probably a specialized structure for our game Map and should be named/organized such
data IndexedImage = IndexedImage {
    colorRegions :: [[(Int, Int)]],
    colorSets    :: [I.IntSet], -- for fast lookup performance
    image        :: Image PixelRGBA8,
    indexRegions :: [((Int, Int), Image PixelRGBA8)]
}

-- get a list of pixel regions containing the same color
-- this rejects fully transparent pixels
indexImage :: Image PixelRGBA8 -> IndexedImage
indexImage i =
    let queue = [ (x,y) | y <- [0..imageHeight i - 1], x <- [0..imageWidth i - 1] ]
        rs = S.evalState (adjacentPixelRegionsState i queue) mempty
    -- let rs = fst (adjacentPixelRegionsF [(0,0)] i mempty mempty)
        s  = map (regionToSet i) rs
    in  IndexedImage rs s i (regionsToImages i rs s )

filterIndex :: ([(Int,Int)] -> I.IntSet -> Bool) -> IndexedImage -> IndexedImage
filterIndex f i = let (rs, s) = unzip $ filter (uncurry f) (zip (colorRegions i) (colorSets i))
                  in  IndexedImage rs s (image i) (indexRegions i)

adjacentPixelRegionsState :: Image PixelRGBA8 -> [(Int, Int)] -> S.State (I.IntSet) [[(Int, Int)]]
adjacentPixelRegionsState _ [] = return []
adjacentPixelRegionsState i ((x,y):queue) = do
        checked <- S.get
        if isVisited (pToInt (imageWidth i) (x,y)) checked || isTransparent (pixelAt i x y) then
            adjacentPixelRegionsState i queue
        else do
            r <- adjacentPixelsState i (x, y)
            (r:) <$> adjacentPixelRegionsState i queue

-- gets the list of adjacent pixels of the same color
adjacentPixels i xy = S.evalState (adjacentPixelsState i xy) mempty

adjacentPixelsState :: Image PixelRGBA8 -> (Int, Int) -> S.State (I.IntSet) [(Int, Int)]
adjacentPixelsState i (x,y) = S.StateT $ \s -> return (adjacentToIntSet s [(x,y)])
    where
        adjacentToIntSet visited []              = ([], visited)
        adjacentToIntSet visited ((x,y):queue)
            | isVisited (pToInt w (x,y)) visited = adjacentToIntSet visited queue 
            | not (pixelInRegion (x,y))          = adjacentToIntSet visited queue -- pixelAt seems to be the next bottleneck
            | otherwise                          =
                let visited' = I.insert (pToInt w (x,y)) visited
                    newps    = filter (isVisitable visited') [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                    queue'   = newps ++ queue
                    (r, s)   = adjacentToIntSet visited' queue'
                in  ((x,y):r, s)

        isVisitable visited (x, y) = not (isVisited (pToInt w (x,y)) visited)
        pixelInRegion (x', y') = not (outOfBounds (x', y') i) && (pixelAt i x' y' == p)
        outOfBounds (x, y) i = x >= w || x < 0 || y >= h || y < 0
        p = pixelAt i x y
        w = imageWidth i
        h = imageHeight i

-- get the index of the color region for a pixel coordinate
findPixel :: IndexedImage -> (Int, Int) -> Maybe Int
findPixel (IndexedImage _ rs i _) p = L.elemIndex True (map ((pToInt (imageWidth i) p) `I.member`) rs)

-- get the intset for the pixels in the regions
regionsToSet :: Image PixelRGBA8 -> [[(Int, Int)]] -> I.IntSet
regionsToSet i = foldr f mempty
    where f r s = s `I.union` regionToSet i r

-- get the intset for the pixels in the region
regionToSet :: Image PixelRGBA8 -> [(Int, Int)] -> I.IntSet
regionToSet i = foldr f mempty
    where f p s = I.insert (pToInt (imageWidth i) p) s

regionsToImages :: Image PixelRGBA8 -> [[(Int, Int)]] -> [I.IntSet] -> [((Int, Int), Image PixelRGBA8)]
regionsToImages img rs is = let len = length rs
                            in  if len == 0 then []
                                else map f [0..len - 1]
    where f i = regionToImage img (is !! i) (rs !! i)

regionToImage :: Image PixelRGBA8 -> I.IntSet -> [(Int, Int)] -> ((Int, Int), Image PixelRGBA8)
regionToImage img s r =
    let (w,h)   = (regionWidth  r, regionHeight r)
        (sx,sy) = (regionStartX r, regionStartY r)
        imageW  = imageWidth img
        f x y   = if pToInt imageW (sx + x, sy + y) `I.member` s then pixelAt img (sx + x) (sy + y)
                  else PixelRGBA8 0 0 0 0
    in  if (w == 0 || h == 0) then error "Width/height must not be zero"
        else ((sx, sy), generateImage f w h)

-- get start xpos of region
regionStartX :: [(Int, Int)] -> Int
regionStartX [] = error "Empty region"
regionStartX r  = let xs = map fst r in minimum xs

-- get start ypos of region
regionStartY :: [(Int, Int)] -> Int
regionStartY [] = error "Empty region"
regionStartY r  = let ys = map snd r in minimum ys

-- get rectangular height of region
regionHeight :: [(Int, Int)] -> Int
regionHeight [] = 0
regionHeight r  = let ys = map snd r in maximum ys - minimum ys + 1

-- get rectangular width of region
regionWidth :: [(Int, Int)] -> Int
regionWidth [] = 0
regionWidth r  = let xs = map fst r in maximum xs - minimum xs + 1

-- get the color associated with the region
regionColor :: Image PixelRGBA8 -> [(Int, Int)] -> Maybe (PixelRGBA8)
regionColor img [] = Nothing
regionColor img ((x,y):xys) = let p = pixelAt img x y
                              in  if isTransparent p then regionColor img xys
                                  else Just p

-- convenience function so we can use IntSet for performance
isVisited :: Int -> I.IntSet -> Bool
isVisited i visited = i `I.member` visited
pToInt :: Int -> (Int, Int) -> Int
pToInt w (x,y) = w*y + x

imageGrid :: Image PixelRGBA8 -> [(Int, Int)]
imageGrid i = let w = imageWidth i
                  h = imageHeight i
              in  [ (x, y) | x <- [0..w], y <- [0..h] ]
