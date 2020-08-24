{-# LANGUAGE TupleSections #-}
module Game.Init where

import Game.Types
import Game.Renderer
import Graphics.Image
import Graphics.Image.Index
import Util

import Codec.Picture (Image(..), PixelRGBA8(..), generateImage, pixelAt)
import Control.Monad (when, forM_, forM)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Char (isDigit, isSpace)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import SDL
import System.Random
import System.Random.Shuffle
import qualified Data.Map as M
import qualified SDL.Font as Font

maxPlayers = 5

initGame :: Int -> Window -> Renderer -> IO (Maybe (RendererEnv, GameState))
initGame playerCnt window renderer = do
    when (playerCnt <= 0 || playerCnt > maxPlayers) (error "Player count must be 1-5")
    Font.initialize
    font   <- Font.load "res/font/LiberationSans-Regular.ttf" 16
    {-- load game assets in separate thread --}
    shared <- atomically $ newTVar Nothing
    forkIO $ do
        {-- load background image --}
        image <- loadImage "res/image/risk-map-white.png"
        putStrLn "Image loaded"
        {-- convert image to SDL --}
        surface  <- createSurfaceFromImage image
        texture  <- createTextureFromSurface renderer surface
        {-- load image without numbers or borders to index regions --}
        image <- loadImage "res/image/risk-map-continent-colors.png"
        putStrLn "Image loaded"
        image' <- loadImage "res/image/risk-map-connected-regions.png"
        putStrLn "Index image loaded"
        let index   = indexImage image'
            {-- the following is necessary so we mask out colors not in original map --}
            images' = map (initRegionImage image index) [0..length (indexRegions index) - 1] 
        -- TODO initialize image regions to default color
        when (length (indexRegions index) /= 42) (error "Unable to continue - territories in index does not equal 42!")
        putStrLn "Image indexed"
        when (length (indexRegions index) /= length images') (error "Unable to continue - territories do not equal mask images!")
        putStrLn "Index masked"
        surfaces <- mapM createSurfaceFromImage images'
        textures <- mapM (createTextureFromSurface renderer) surfaces
        putStrLn "Region textures loaded"
        {-- initialize territories & game state --}
        let ts      = initTerritories index image
            texMap  = M.fromList (zip ts textures)
            players = map (\n -> if n <= playerCnt then Player n else Neutral n) [1..maxPlayers]
        conns    <- getConnections ts
        occupied <- initOccupied ts players
        forM_ (invalidConnections conns) $ \f -> do
            putStrLn ("Failed connection for region " ++ show (territoryLoc f))
        {-- construct the env and game state --}
        let env      = RendererEnv window renderer texture font index texMap
            st       = GameState Nothing players (Move Nothing) Nothing ts conns occupied []
        atomically $ writeTVar shared (Just (env, st))
    {-- wait to play game until assets are loaded --}
    waitUntilLoaded window renderer font shared
    atomically $ readTVar shared

--waitUntilLoaded :: Renderer -> Font.Font -> TVar (Maybe RendererEnv) -> IO ()
waitUntilLoaded win r f shared = do
    index <- atomically $ readTVar shared
    if isJust index then return ()
    else do
        renderLoadingScreen win r f
        waitUntilLoaded win r f shared

initTerritories :: IndexedImage -> Image PixelRGBA8 -> [Territory]
initTerritories index bgimg = map initTerritory [0..length points - 1]
    where points = map (fromXY . fst) (indexRegions index)
          initTerritory r = Territory (points !! r) cont
              where cont      = case contType of
                                  Just t  -> Continent t (fromXY (0,0))
                                  Nothing -> error ("Invalid region color: " ++ show tColor)
                    tColor    = regionColor bgimg (colorRegions index !! r) 
                    contType  = tColor >>= continentType
                    region    = colorRegions index !! r
                    (x,y)     = (regionStartX region, regionStartY region)
                    (w,h)     = (regionWidth  region, regionHeight region)
                    numberLoc = fromXY (x + floor ((fromIntegral w)/2), y + floor ((fromIntegral h)/2))

getConnections :: [Territory] -> IO (Map Territory [Territory])
getConnections ts = M.fromList . zip ts <$> mapM (getTConnections ts) [0..length ts - 1]

-- load territory connections from file
getTConnections :: [Territory] -> Int -> IO [Territory]
getTConnections ts tid = do
        ls <- filter findMatchingLine . lines <$> readFile "res/territory-connections"
        if length ls == 0 then do
            putStrLn ("Unable to find connections for territory " ++ show tid)
            return []
        else
            let connIds = parseConnection $ head ls
            in  return (map (\tid -> ts !! tid) connIds)
    where
        findMatchingLine l = let digits = takeWhile isDigit l
                             in  if length digits > 0 then read digits == tid
                                 else False
        parseConnection    = map read . splitDigits . dropWhile isSpace . dropWhile isDigit
        splitDigits        = words . map (\c -> if isDigit c then c else ' ')

invalidConnections :: Map Territory [Territory] -> [Territory]
invalidConnections conns = let ts = M.keys conns
                           in  filter (not . checkConnections conns) ts
    where
        checkConnections :: Map Territory [Territory] -> Territory -> Bool
        checkConnections tconns t =
            let conns = fromMaybe (err t) (M.lookup t tconns)
                f t'  = t `elem` (fromMaybe (err t') (M.lookup t' tconns))
                err i = error ("Unable to find " ++ show (territoryLoc i))
            in  all f conns

initOccupied :: [Territory] -> [Player] -> IO (Map Territory (Player, Int))
initOccupied ts ps = do
        g <- newStdGen
        let (shuffledTs, _) = shuffle g ts
            balancedTs      = reverse (splitBalanced (length ps) shuffledTs)
        when (length balancedTs /= length ps) (error "Split error while shuffling territories!")
        return (mkMap balancedTs ps)
    where mkMap balancedTs ps = let f (p,ts) m = foldr (g p) m ts
                                    g  p t   m = M.insert t (p, 1) m
                                in  foldr f mempty (zip ps balancedTs)
                               -- let l = zip balancedTs (map (,1) ps)
                               --     f = \(k,v) -> zip balancedTs (repeat vals)
                               -- in  M.fromList (map f l)
    
initRegionImage :: Image PixelRGBA8 -> IndexedImage -> Int -> Image PixelRGBA8
initRegionImage bgimg index r = let ((sx,sy),i) = indexRegions index !! r
                                    (w,h) = (imageWidth i, imageHeight i)
                                in  generateImage (g r (sx,sy)) w h :: Image PixelRGBA8
    where g r (sx,sy) x y = let pi        = pixelAt bgimg (sx + x) (sy + y)
                                r'        = findPixel index (sx + x, sy + y)
                                inCountry = not (isTransparent pi || pi == black)
                            in  case r' of
                                    (Just r') | r' == r && inCountry
                                                           -> PixelRGBA8 255 255 255 255
                                              | otherwise  -> PixelRGBA8 0   0   0   0
                                    Nothing                -> PixelRGBA8 0   0   0   0
