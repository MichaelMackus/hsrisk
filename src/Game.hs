module Game (gameLoop, runGame, indexFilter) where

import Game.Types
import Game.Renderer
import Graphics.Image

import Codec.Picture (Image(..), PixelRGBA8(..), pixelOpacity)
import Control.Monad.Reader
import SDL
import qualified Control.Monad.State as State

runGame :: Window -> Renderer -> Image PixelRGBA8 -> IO ()
runGame window renderer image = do
    {-- convert image to SDL --}
    surface  <- createSurfaceFromImage image
    texture  <- createTextureFromSurface renderer surface
    -- get texture regions from image index
    let index = indexImage indexFilter image
    seq index (putStrLn "Image indexed")
    regions  <- createTexturesFromIndex renderer index
    -- TODO group small closeby regions (i.e. islands)
    -- TODO initialize image regions to default color
    State.evalStateT (runReaderT gameLoop (RendererEnv window renderer texture index regions)) (GameState True Nothing) 

indexFilter :: PixelRGBA8 -> Bool
indexFilter p = not (isTransparent p || isBlack p)
    where
      isTransparent :: PixelRGBA8 -> Bool
      isTransparent = (==0) . pixelOpacity
      isBlack :: PixelRGBA8 -> Bool
      isBlack (PixelRGBA8 0 0 0 255) = True
      isBlack otherwise = False

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
