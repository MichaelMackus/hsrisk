module Game (gameLoop, runGame) where

import Game.Init
import Game.Types
import Game.Renderer
import Game.Types
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
            n <- assignableUnits
            changePhase (Assign n)
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
handleEvent (MouseButtonEvent e) = do
  let (x, y) = toXY (mouseButtonEventPos e)
  i  <- asks index
  ts <- State.gets territories
  case findPixel i (x,y) of
    Nothing  -> return ()
    Just tid -> do
        let t = ts !! tid
        phase <- State.gets phase
        case phase of
            (Assign 0) -> changePhase Attack
            (Assign n) -> do
                p  <- return . fromJust =<< State.gets playing
                ts <- State.gets occupiedTerritories
                let (p', occupied) = fromJust (M.lookup t ts)
                when (p == p') $ do
                    let occupied' = M.insert t (p, occupied + 1) ts
                    State.modify $ \s -> s {
                        phase = (Assign (n - 1)),
                        occupiedTerritories = occupied'
                    }
            otherwise  -> return ()
handleEvent (KeyboardEvent e)
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeQ = liftGame gquit
handleEvent (WindowClosedEvent _) = liftGame gquit
handleEvent otherwise = return ()
