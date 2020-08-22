module Game (gameLoop, runGame) where

import Game.Init
import Game.Types
import Game.Renderer
import Game.Types
import Graphics.Image
import Util

import Control.Monad.Reader
import Data.Maybe (isJust)
import SDL
import qualified Control.Monad.State as State

runGame :: Window -> Renderer -> IO ()
runGame window renderer = do
    (Just (env, st)) <- initGame 2 window renderer
    putStrLn "Asset loading thread finished"
    State.evalStateT (runReaderT gameLoop env) st

gameLoop :: GameRenderer ()
gameLoop = do
  {-- handle events --}
  events  <- map eventPayload <$> liftIO pollEvents
  mapM_ handleEvent events
  updateRenderer
  {-- render to screen & continue playing --}
  liftIO . present =<< asks renderer
  playing <- State.gets playing
  when (isJust playing) gameLoop

handleEvent :: EventPayload -> GameRenderer ()
-- handleEvent (MouseButtonEvent  e)
handleEvent (MouseMotionEvent e) = do
  let (x, y) = toXY (mouseMotionEventPos e)
  i  <- asks index
  ts <- State.gets territories
  case findPixel i (x,y) of
    Just tid -> State.modify (\s -> s { hovering = Just (ts !! tid) })
    Nothing  -> State.modify (\s -> s { hovering = Nothing })
handleEvent (KeyboardEvent e)
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeQ = liftGame gquit
handleEvent (WindowClosedEvent _) = liftGame gquit
handleEvent otherwise = return ()
