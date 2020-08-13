module Graphics.Image.Index
    (IndexedImage(..)
     ,indexImage
     ,findPixel
    ) where

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
    colorRegions :: [I.IntSet],
    image :: Image PixelRGBA8
}

-- get a list of pixel regions containing the same color
-- this rejects fully transparent pixels
indexImage :: Image PixelRGBA8 -> IndexedImage
indexImage i =
    let rs = map (regionToSet i) . filter (not . null) $ S.evalState (adjacentPixelRegionsState [(0,0)] i mempty) mempty
    in  seq (length rs) $ IndexedImage rs i

-- TODO move queue to end of list (see: wiki.haskell.org/Parameter_order)
-- TODO fix performance
-- TODO make it index all colors near each other without other colors in between (disregarding black/alpha)
--       this way, for example, the islands near australia all collect into a single region
adjacentPixelRegionsState :: [(Int, Int)] -> Image PixelRGBA8 -> I.IntSet -> S.State (I.IntSet) [[(Int, Int)]]
adjacentPixelRegionsState [] i _ = return []
adjacentPixelRegionsState ((x,y):queue) i visited = do
            if isVisited (pint (x,y) i) visited then adjacentPixelRegionsState queue i visited
            else do
                checked <- S.get
                if not (outOfBounds (x,y) i) then do
                    s  <- if isVisited (pint (x,y) i) checked then return [] else checkPixel (x, y)
                    let queue' = (x+1, y):(x-1, y):(x, y+1):(x, y-1):queue
                    (s:) <$> (adjacentPixelRegionsState queue' i $ I.insert (pint (x,y) i) visited)
                else
                    adjacentPixelRegionsState queue i visited
    where checkPixel (x, y) =
                if isTransparent (pixelAt i x y) || isBlack (pixelAt i x y) then do
                   S.modify $ I.insert (pint (x,y) i)
                   return []
                else adjacentPixelsState (x,y) i

          isTransparent :: PixelRGBA8 -> Bool
          isTransparent = (==0) . pixelOpacity

          isBlack :: PixelRGBA8 -> Bool
          isBlack (PixelRGBA8 0 0 0 255) = True
          isBlack otherwise = False

-- gets the list of adjacent pixels of the same color
adjacentPixels xy i = S.evalState (adjacentPixelsState xy i) mempty

adjacentPixelsState :: (Int, Int) -> Image PixelRGBA8 -> S.State (I.IntSet) [(Int, Int)]
adjacentPixelsState (x,y) i = S.StateT $ \s -> return (adjacentToIntSet [(x,y)] s)
    where
        adjacentToIntSet [] visited          = ([], visited)
        adjacentToIntSet ((x,y):queue) visited
            | not (isPixel (x,y))             = adjacentToIntSet queue visited -- isPixel seems to be the next bottleneck
            | pint (x,y) i `I.member` visited = adjacentToIntSet queue visited
            | otherwise                       =
                let visited' = I.insert (pint (x,y) i) visited
                    queue'   = (x+1,y):(x-1,y):(x,y+1):(x,y-1):queue
                    (r, s)   = adjacentToIntSet queue' visited'
                in  ((x,y):r, s)

        p = pixelAt i x y
        isPixel (x, y) = not (outOfBounds (x, y) i) && pixelAt i x y == p


-- get the index of the color region for a pixel coordinate
findPixel :: IndexedImage -> (Int, Int) -> Maybe Int
findPixel (IndexedImage rs i) p = L.elemIndex True (map ((pint p i) `I.member`) rs)

-- get the intset for the pixels in the regions
regionsToSet :: Image PixelRGBA8 -> [[(Int, Int)]] -> I.IntSet
regionsToSet i = foldr f mempty
    where f r s = s `I.union` regionToSet i r

-- get the intset for the pixels in the region
regionToSet :: Image PixelRGBA8 -> [(Int, Int)] -> I.IntSet
regionToSet i = foldr f mempty
    where f p s = I.insert (pint p i) s

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
regionHeight r  = let ys = map snd r in maximum ys - minimum ys

-- get rectangular width of region
regionWidth :: [(Int, Int)] -> Int
regionWidth [] = 0
regionWidth r  = let xs = map fst r in maximum xs - minimum xs

-- convenience function so we can use IntSet for performance
isVisited :: Int -> I.IntSet -> Bool
isVisited i visited = i `I.member` visited
pint :: (Int, Int) -> Image a -> Int
pint (x,y) i = (imageWidth i)*y + x

outOfBounds :: (Int, Int) -> Image a -> Bool
outOfBounds (x, y) i = x >= imageWidth  i || x < 0 || y >= imageHeight i || y < 0

imageGrid :: Image PixelRGBA8 -> [(Int, Int)]
imageGrid i = let w = imageWidth i
                  h = imageHeight i
              in  [ (x, y) | x <- [0..w], y <- [0..h] ]
