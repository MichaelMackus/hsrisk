module Game (gameLoop, runGame) where

import Game.Types
import Game.Renderer
import Graphics.Image
import Graphics.Image.Util

import Codec.Picture (Image(..), PixelRGBA8(..), pixelOpacity, generateImage, pixelAt)
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe (isJust)
import SDL
import qualified SDL.Font as Font
import qualified Control.Monad.State as State
import qualified Data.IntSet as S

runGame :: Window -> Renderer -> IO ()
runGame window renderer = do
    Font.initialize
    font     <- Font.load "res/font/LiberationSans-Regular.ttf" 16
    {-- load game assets in separate thread --}
    shared <- atomically $ newTVar Nothing
    forkIO $ do
        {-- load background image --}
        image <- loadImage "res/image/Risk_game_map_fixed.png"
        putStrLn "Image loaded"
        {-- convert image to SDL --}
        surface  <- createSurfaceFromImage image
        texture  <- createTextureFromSurface renderer surface
        {-- load image without numbers or borders to index regions --}
        image' <- loadImage "res/image/risk-map-connected-regions.png"
        putStrLn "Index image loaded"
        let index   = indexImage image'
            images' = map f (indexRegions index) -- this is necessary so we mask out colors not in original map
                where f ((sx,sy),i)   = let (w,h) = (imageWidth i, imageHeight i)
                                        in  generateImage (g (sx,sy) i) w h :: Image PixelRGBA8
                      g (sx,sy) i x y = let pi   = pixelAt image (sx + x) (sy + y)
                                            pi'  = pixelAt i x y
                                        in  if isTransparent pi || pi /= pi' then PixelRGBA8 0 0 0 0
                                            else PixelRGBA8 255 255 255 125
        -- TODO initialize image regions to default color
        when (length (indexRegions index) /= 42) (error "Unable to continue - territories in index does not equal 42!")
        putStrLn "Image indexed"
        when (length (indexRegions index) /= length images') (error "Unable to continue - territories do not equal mask images!")
        putStrLn "Index masked"
        surfaces <- mapM createSurfaceFromImage images'
        textures <- mapM (createTextureFromSurface renderer) surfaces
        putStrLn "Region textures loaded"
        let regions = zip (regionRects index) textures
            env     = RendererEnv window renderer texture index regions
        atomically $ writeTVar shared (Just env)
    {-- wait to play game until assets are loaded --}
    waitUntilLoaded renderer font shared $ do
        putStrLn "Asset loading thread finished"
        (Just env) <- atomically $ readTVar shared
        State.evalStateT (runReaderT gameLoop env) (GameState True Nothing) 

waitUntilLoaded :: Renderer -> Font.Font -> TVar (Maybe RendererEnv) -> IO () -> IO ()
waitUntilLoaded r f shared finally = do
    index <- atomically $ readTVar shared
    if isJust index then finally
    else do
        renderLoadingScreen r f
        waitUntilLoaded r f shared finally

gameLoop :: GameRenderer ()
gameLoop = do
  {-- handle events --}
  events  <- map eventPayload <$> liftIO pollEvents
  mapM_ handleEvent events
  updateRenderer
  {-- render to screen & continue playing --}
  liftIO . present =<< asks renderer
  playing <- State.gets playing
  when playing gameLoop

handleEvent :: EventPayload -> GameRenderer ()
-- handleEvent (MouseButtonEvent  e)
handleEvent (MouseMotionEvent e) = do
  let (x, y) = toXY (mouseMotionEventPos e)
  i <- asks index
  case findPixel i (x,y) of
    Just r  -> State.modify (\s -> s { region = Just r })
    Nothing -> State.modify (\s -> s { region = Nothing })
handleEvent (KeyboardEvent e)
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeQ = liftGame gquit
handleEvent (WindowClosedEvent _) = liftGame gquit
handleEvent otherwise = return ()
