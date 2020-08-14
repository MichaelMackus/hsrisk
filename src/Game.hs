module Game (gameLoop, runGame, indexFilter) where

import Game.Types
import Game.Renderer
import Graphics.Image

import Codec.Picture (Image(..), PixelRGBA8(..), pixelOpacity)
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import SDL
import qualified Control.Monad.State as State
import qualified Data.IntSet as S

runGame :: Window -> Renderer -> Image PixelRGBA8 -> IO ()
runGame window renderer image = do
    {-- convert image to SDL --}
    surface  <- createSurfaceFromImage image
    texture  <- createTextureFromSurface renderer surface
    -- get texture regions from image index
    shared <- atomically $ newTVar Nothing
    forkIO $ do
        let index = filterIndex f (indexImage indexFilter image)
            -- for now we just drop the small regions
            -- TODO group small closeby regions (i.e. islands)
            f r i = let s = S.size i
                    in  s > 600 || regionStartX r < 700 ||
                         (regionStartY r < 300 && s > 200)
            -- TODO initialize image regions to default color
        seq (length $ colorRegions index) (putStrLn "Image indexed")
        atomically $ writeTVar shared (Just index)
    waitUntilLoaded renderer shared $ do
        putStrLn "Thread finished"
        (Just index) <- atomically $ readTVar shared
        regions      <- createTexturesFromIndex renderer index
        State.evalStateT (runReaderT gameLoop (RendererEnv window renderer texture index regions)) (GameState True Nothing) 

indexFilter :: PixelRGBA8 -> Bool
indexFilter p = not (isTransparent p || isBlack p)
    where
      isTransparent :: PixelRGBA8 -> Bool
      isTransparent = (==0) . pixelOpacity
      isBlack :: PixelRGBA8 -> Bool
      isBlack (PixelRGBA8 0 0 0 255) = True
      isBlack otherwise = False

waitUntilLoaded :: Renderer -> TVar (Maybe IndexedImage) -> IO () -> IO ()
waitUntilLoaded r shared finally = do
    index <- atomically $ readTVar shared
    case index of
        Nothing -> do
            renderLoadingScreen r
            waitUntilLoaded r shared finally
        Just _  -> finally

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
