module Game (gameLoop, runGame) where

import Game.Attack
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
  p  <- return . fromJust =<< State.gets playing
  case findPixel i (x,y) of
    Nothing  -> return ()
    Just tid -> do
        let t = ts !! tid
        phase <- State.gets phase
        case phase of
            (Assign n) -> do
                ts <- State.gets occupiedTerritories
                let (p', occupied) = fromJust (M.lookup t ts)
                when (p == p') $ do
                    let occupied' = M.insert t (p, occupied + 1) ts
                    State.modify $ \s -> s {
                        phase = (Assign (n - 1)),
                        occupiedTerritories = occupied'
                    }
                when (n == 1) advanceTurn
            (Attack Nothing) -> do
                ts <- State.gets occupiedTerritories
                let (p', occupied) = fromJust (M.lookup t ts)
                when (p == p') $ do
                    newMessage "Choose an adjacent territory to attack... (ESCAPE aborts)"
                    State.modify $ \s -> s { phase = (Attack (Just t)) }
            (Attack (Just from)) -> attack from t
            otherwise  -> return ()
handleEvent (KeyboardEvent e)
    -- quit on "q"
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeQ = liftGame gquit
    -- abort attack/move on ESCAPE
    | keyboardEventKeyMotion e == Pressed &&
      keysymKeycode (keyboardEventKeysym e) == KeycodeEscape = do
        phase <- State.gets phase
        case phase of
            (Attack (Just _)) -> State.modify $ \s -> s { phase = Attack Nothing }
            (Move   (Just _)) -> State.modify $ \s -> s { phase = Move Nothing }
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
