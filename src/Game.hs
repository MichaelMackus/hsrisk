module Game (gameLoop, runGame) where

import Game.Init
import Game.Types
import Game.Renderer
import Graphics.Image
import Graphics.Image.Util

import Control.Monad.Reader
import SDL
import qualified Control.Monad.State as State

runGame :: Window -> Renderer -> IO ()
runGame window renderer = do
    (Just env) <- initGame window renderer
    putStrLn "Asset loading thread finished"
    State.evalStateT (runReaderT gameLoop env) (GameState True Nothing) 

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
