module Game.Types where

import Graphics.Image.Index
import Graphics.Rect

import Control.Monad.Reader
import Codec.Picture (PixelRGBA8(..))
import Data.Maybe (fromMaybe)
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
  phase   :: Phase,
  hovering :: Maybe Territory,
  territories :: [Territory],
  territoryConnections :: Map Territory [Territory],
  occupiedTerritories :: Map Territory (Player, Int),
  messages :: [String]
}

data Player = Player Int | Neutral Int deriving Eq
data Phase = Assign Int | Attack | Move deriving Eq

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

newMessage :: String -> GameRenderer ()
newMessage msg = do
    msgs <- State.gets messages
    State.modify (\s -> s { messages = msg:msgs })

changePhase :: Phase -> GameRenderer ()
changePhase p = do
    State.modify (\s -> s { phase = p })
    case p of
        (Assign n) -> newMessage ("You get " ++ show n ++ " units! Assign them to your territories." )
        Attack     -> newMessage ("Attack phase - choose territory to attack from, then choose a target. Enter when done.")
        Move       -> newMessage ("Move phase - choose territory to move from, then choose a target. Enter when done.")

-- get new assignable units
assignableUnits :: GameRenderer Int
assignableUnits = do
    p  <- fromMaybe (error "No player") <$> State.gets playing
    ts <- playerTerritories p
    let def = 3
    return def

playerTerritories :: Player -> GameRenderer [Territory]
playerTerritories p = let f t (p',_) ts = if p == p' then (t:ts)
                                          else ts
                      in  return . M.foldrWithKey f [] =<< State.gets occupiedTerritories

continentType :: PixelRGBA8 -> Maybe (ContinentType)
continentType (PixelRGBA8 255 255 0   255) = Just NAmerica
continentType (PixelRGBA8 255 0   0   255) = Just SAmerica
continentType (PixelRGBA8 0   0   255 255) = Just Europe
continentType (PixelRGBA8 128 64  0   255) = Just Africa
continentType (PixelRGBA8 0   164 0   255) = Just Asia
continentType (PixelRGBA8 128 0   255 255) = Just Australia
continentType otherwise = Nothing

continentColor :: ContinentType -> PixelRGBA8
continentColor NAmerica = PixelRGBA8 255 255 0   255
continentColor SAmerica = PixelRGBA8 255 0   0   255
continentColor Europe = PixelRGBA8 0   0   255 255
continentColor Africa = PixelRGBA8 128 64  0   255
continentColor Asia = PixelRGBA8 0   164 0   255
continentColor Australia = PixelRGBA8 128 0   255 255

playerColor :: Player -> PixelRGBA8
playerColor p = case p of
                    (Player  n) -> color n 255
                    (Neutral n) -> color n 125
    where color 1 = PixelRGBA8 255 0   0
          color 2 = PixelRGBA8 128 0   255
          color 3 = PixelRGBA8 0   255 0
          color 4 = PixelRGBA8 0   0   255
          color 5 = PixelRGBA8 255 255 0
          color _ = error "Invalid player number"
