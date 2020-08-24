module Game (gameLoop, runGame) where

import Game.Init
import Game.Types
import Game.Renderer
import Game.Types
import Game.Phase
import Graphics.Image
import Util

import Control.Monad.Reader
import Data.Maybe (isJust, isNothing, fromJust)
import SDL
import qualified Control.Monad.State as State
import qualified Data.Map as M

runGame :: Window -> Renderer -> IO ()
runGame window renderer = do
        (Just (env, st)) <- initGame 2 window renderer
        putStrLn "Asset loading thread finished"
        State.evalStateT (runReaderT startGame env) st
    where
        startGame = do
            newMessage "New game"
            advanceTurn -- cheap hack to show "It is now player 1" at start of game
            gameLoop

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
handleEvent (MouseButtonEvent e) | mouseButtonEventMotion e == Released = do
  i  <- asks index
  ts <- State.gets territories
  p  <- return . fromJust =<< State.gets playing
  let (x, y) = toXY (mouseButtonEventPos e)
  case findPixel i (x,y) of
    Nothing  -> return ()
    Just tid -> interactWithTerritory p (ts !! tid) =<< State.gets phase
handleEvent (KeyboardEvent e)
    -- quit on "q"
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeQ = liftGame gquit
    -- abort attack/move on ESCAPE
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeEscape = do
        phase <- State.gets phase
        let abortMsg = newMessage "Aborting previous action"
        case phase of
            (Attack (Just _)) -> abortMsg >> State.modify (\s -> s { phase = Attack Nothing })
            (Move   (Just _)) -> abortMsg >> State.modify (\s -> s { phase = Move Nothing })
            otherwise         -> return ()
    -- advance to next phase on ENTER
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeReturn = do
        phase <- State.gets phase
        case phase of
            (Attack _) -> advanceTurn
            (Move   _) -> advanceTurn
            otherwise  -> return () -- must assign all new units first
handleEvent (WindowClosedEvent _) = liftGame gquit
handleEvent otherwise = return ()
