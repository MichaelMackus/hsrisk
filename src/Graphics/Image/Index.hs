module Graphics.Image.Index
    (IndexedImage(..)
     ,indexImage
     ,findPixel
     ,regionStartX
     ,regionStartY
     ,regionHeight
     ,regionWidth
     ,pToInt
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
    colorRegions :: [[(Int, Int)]],
    colorSets    :: [I.IntSet], -- for fast lookup performance
    image :: Image PixelRGBA8
}

-- get a list of pixel regions containing the same color
-- this rejects fully transparent pixels
indexImage :: (PixelRGBA8 -> Bool) -> Image PixelRGBA8 -> IndexedImage
indexImage f i =
    let rs = filter (not . null) $ S.evalState (adjacentPixelRegionsState f [(0,0)] i mempty) mempty
        s  = map (regionToSet i) rs
    in  seq (length rs) $ IndexedImage rs s i

-- TODO move queue to end of list (see: wiki.haskell.org/Parameter_order)
-- TODO fix performance
-- TODO make it index all colors near each other without other colors in between (disregarding black/alpha)
--       this way, for example, the islands near australia all collect into a single region
adjacentPixelRegionsState :: (PixelRGBA8 -> Bool) -> [(Int, Int)] -> Image PixelRGBA8 -> I.IntSet -> S.State (I.IntSet) [[(Int, Int)]]
adjacentPixelRegionsState _ [] _ _ = return []
adjacentPixelRegionsState f ((x,y):queue) i visited = do
            if isVisited (pToInt (imageWidth i) (x,y)) visited then adjacentPixelRegionsState f queue i visited
            else do
                checked <- S.get
                if not (outOfBounds (x,y) i) then do
                    s  <- if isVisited (pToInt (imageWidth i) (x,y)) checked then return [] else checkPixel (x, y)
                    let queue' = (x+1, y):(x-1, y):(x, y+1):(x, y-1):queue
                    (s:) <$> (adjacentPixelRegionsState f queue' i $ I.insert (pToInt (imageWidth i) (x,y)) visited)
                else
                    adjacentPixelRegionsState f queue i visited
    where checkPixel (x, y) =
                if not (f (pixelAt i x y)) then do
                   S.modify $ I.insert (pToInt (imageWidth i) (x,y))
                   return []
                else adjacentPixelsState (x,y) i

-- gets the list of adjacent pixels of the same color
adjacentPixels xy i = S.evalState (adjacentPixelsState xy i) mempty

adjacentPixelsState :: (Int, Int) -> Image PixelRGBA8 -> S.State (I.IntSet) [(Int, Int)]
adjacentPixelsState (x,y) i = S.StateT $ \s -> return (adjacentToIntSet [(x,y)] s)
    where
        adjacentToIntSet [] visited              = ([], visited)
        adjacentToIntSet ((x,y):queue) visited
            | not (pixelInRegion (x,y))          = adjacentToIntSet queue visited -- isPixel seems to be the next bottleneck
            | isVisited (pToInt w (x,y)) visited = adjacentToIntSet queue visited
            | otherwise                          =
                let visited' = I.insert (pToInt w (x,y)) visited
                    queue'   = (x+1,y):(x-1,y):(x,y+1):(x,y-1):queue
                    (r, s)   = adjacentToIntSet queue' visited'
                in  ((x,y):r, s)

        pixelInRegion (x', y') = not (outOfBounds (x', y') i) && (pixelAt i x' y' == pixelAt i x y)
                                    -- TODO isTransparent won't *quite* do it since it will loop around *all* transparent pixels
                                    -- (pixelAt i x' y' == pixelAt i x y || isTransparent pixelAt i x' y')
        w = imageWidth i

-- get the index of the color region for a pixel coordinate
findPixel :: IndexedImage -> (Int, Int) -> Maybe Int
findPixel (IndexedImage _ rs i) p = L.elemIndex True (map ((pToInt (imageWidth i) p) `I.member`) rs)

-- get the intset for the pixels in the regions
regionsToSet :: Image PixelRGBA8 -> [[(Int, Int)]] -> I.IntSet
regionsToSet i = foldr f mempty
    where f r s = s `I.union` regionToSet i r

-- get the intset for the pixels in the region
regionToSet :: Image PixelRGBA8 -> [(Int, Int)] -> I.IntSet
regionToSet i = foldr f mempty
    where f p s = I.insert (pToInt (imageWidth i) p) s

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

-- convenience function so we can use IntSet for performance
isVisited :: Int -> I.IntSet -> Bool
isVisited i visited = i `I.member` visited
pToInt :: Int -> (Int, Int) -> Int
pToInt w (x,y) = w*y + x

outOfBounds :: (Int, Int) -> Image a -> Bool
outOfBounds (x, y) i = x >= imageWidth  i || x < 0 || y >= imageHeight i || y < 0

imageGrid :: Image PixelRGBA8 -> [(Int, Int)]
imageGrid i = let w = imageWidth i
                  h = imageHeight i
              in  [ (x, y) | x <- [0..w], y <- [0..h] ]
