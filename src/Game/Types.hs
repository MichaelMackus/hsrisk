module Game.Types where

import Graphics.Image.Index

import Control.Monad.Reader
import Data.Functor.Identity (runIdentity)
import Foreign.C.Types
import SDL
import qualified Control.Monad.State as State
import qualified SDL.Font as Font

type Game = State.State GameState
type GameRenderer = ReaderT RendererEnv (State.StateT GameState IO)

liftGame :: Game a -> GameRenderer a
liftGame g = do
  s <- State.get
  let (r, s') = runIdentity (State.runStateT g s)
  State.put s'
  return r

data RendererEnv = RendererEnv {
  window :: Window,
  renderer :: Renderer,
  background :: Texture,
  font :: Font.Font,
  index :: IndexedImage,
  territories :: [Territory]
}

data Territory = Territory {
  continent   :: Continent,
  connectedTo :: [Territory],
  tRenderData :: (Rectangle CInt, Texture),
  tNumberLoc  :: Point V2 CInt
} deriving Eq

data Continent = Continent {
  ctype          :: ContinentType,
  cAnnotationLoc :: Point V2 CInt
} deriving Eq
data ContinentType = NAmerica | SAmerica | Europe | Asia | Africa | Australia deriving Eq

data GameState = GameState {
  playing :: Bool, region :: Maybe Int
}

gquit :: Game ()
gquit = State.modify (\s -> s { playing = False })
