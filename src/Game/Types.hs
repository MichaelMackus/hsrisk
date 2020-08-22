module Game.Types where

import Graphics.Image.Index
import Graphics.Rect

import Control.Monad.Reader
import Data.Functor.Identity (runIdentity)
import Foreign.C.Types
import SDL
import qualified Control.Monad.State as State
import qualified Data.Map as M
import qualified SDL.Font as Font

type Map k v = M.Map k v

type Game = State.State GameState
type GameRenderer = ReaderT RendererEnv (State.StateT GameState IO)

data RendererEnv = RendererEnv {
  window :: Window,
  renderer :: Renderer,
  background :: Texture,
  font :: Font.Font,
  index :: IndexedImage,
  tRenderData :: Map Territory Texture
}

data GameState = GameState {
  playing :: Maybe Player,
  players :: [Player],
  hovering :: Maybe Territory,
  territories :: [Territory],
  territoryConnections :: Map Territory [Territory],
  occupiedTerritories :: Map Territory (Player, Int)
}

data Player = Player Int | Neutral Int deriving Eq
instance Show Player where
    show (Player  n) = "P" ++ show n
    show (Neutral n) = "N" ++ show n

data Territory = Territory {
  territoryLoc :: Point V2 CInt,
  continent    :: Continent
} deriving (Eq, Ord)

data Continent = Continent {
  ctype          :: ContinentType,
  cAnnotationLoc :: Point V2 CInt
} deriving (Eq, Ord)
data ContinentType = NAmerica | SAmerica | Europe | Asia | Africa | Australia deriving (Eq, Ord, Show)

liftGame :: Game a -> GameRenderer a
liftGame g = do
  s <- State.get
  let (r, s') = runIdentity (State.runStateT g s)
  State.put s'
  return r

gquit :: Game ()
gquit = State.modify (\s -> s { playing = Nothing })

territoryTex :: Territory -> GameRenderer (Texture)
territoryTex t = do
    texMap <- asks tRenderData
    let tex = M.lookup t texMap
    return (maybe (error "Invalid territory!") id tex)

territoryRect :: Territory -> GameRenderer (Rectangle CInt)
territoryRect t = do
    let loc = territoryLoc t
    tex     <- territoryTex t
    texInfo <- queryTexture tex
    return (Rectangle loc (textureDimensions texInfo))
    
connectedTo :: Territory -> GameRenderer [Territory]
connectedTo t = do
    conns <- M.lookup t <$> State.gets territoryConnections
    return (maybe [] id conns)
