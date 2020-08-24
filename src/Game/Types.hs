module Game.Types where

import Graphics.Image.Index
import Graphics.Rect
import Util.Pathfinder (findPathSimple)

import Control.Monad.Reader
import Codec.Picture (PixelRGBA8(..))
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Map ((!))
import Data.Functor.Identity (runIdentity)
import Foreign.C.Types
import SDL
import qualified Control.Monad.State as State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
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
data Phase = NewGame | Assign Int | Attack (Maybe Territory) | Move (Maybe Territory) deriving Eq -- TODO Add phase for moving after attack

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

isHuman :: Player -> Bool
isHuman (Player  _) = True
isHuman (Neutral _) = False

-- get new assignable units
-- TODO account for new territories
assignableUnits :: GameRenderer Int
assignableUnits = do
    p  <- fromMaybe (error "No player") <$> State.gets playing
    ts <- playerTerritories p
    cs <- playerContinents p
    let unitsForTerritories = max 3 (floor (fromIntegral (length ts) / 3))
        unitsForContinents  = foldr (\c n -> n + continentValue (ctype c)) 0 cs
    return (unitsForTerritories + unitsForContinents)

playerTerritories :: Player -> GameRenderer [Territory]
playerTerritories p = let f t (p',_) ts = if p == p' then (t:ts)
                                          else ts
                      in  return . M.foldrWithKey f [] =<< State.gets occupiedTerritories

playerContinents :: Player -> GameRenderer [Continent]
playerContinents p = do
        allTs    <- L.groupBy groupf . L.sortBy sortf <$> allTerritories
        playerTs <- L.groupBy groupf . L.sortBy sortf <$> playerTerritories p
        return (catMaybes . map tsToCont . filter (\ts -> ts `elem` allTs) $ playerTs)
    where groupf    t t' = ctype (continent t) == ctype (continent t')
          sortf     t t' = compare (ctype (continent t)) (ctype (continent t'))
          tsToCont []    = Nothing
          tsToCont (t:_) = Just (continent t)

          -- make things easy and get territories in same order as occupied
          allTerritories = return . M.foldrWithKey (\t _ ts -> (t:ts)) [] =<< State.gets occupiedTerritories

playerNum :: Player -> Int
playerNum (Player  n) = n
playerNum (Neutral n) = n

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

continentValue :: ContinentType -> Int
continentValue NAmerica = 5
continentValue SAmerica = 2
continentValue Europe = 5
continentValue Africa = 3
continentValue Asia = 7
continentValue Australia = 2

playerColor :: Player -> PixelRGBA8
playerColor p = case p of
                    (Player  n) -> color n 255
                    (Neutral n) -> color n 125
    where color 1 = PixelRGBA8 255 0   0
          color 2 = PixelRGBA8 128 0   255
          color 3 = PixelRGBA8 0   255 0
          color 4 = PixelRGBA8 0   255 255
          color 5 = PixelRGBA8 255 255 0
          color _ = error "Invalid player number"

occupyingPlayer :: Map Territory (Player, Int) -> Territory -> Player
occupyingPlayer m t = let (p, _) = m ! t
                      in  p

isAdjacent :: Map Territory [Territory]
                -> Territory -> Territory -> Bool
isAdjacent connsMap from to =
    case M.lookup from connsMap of
        Nothing    -> error "Unable to find connections for territory!"
        Just conns -> any (== to) conns

isConnected :: Map Territory [Territory]
                -> Map Territory (Player, Int)
                -> Territory -> Territory -> Bool
isConnected conns occupied from to =
    let p   = occupyingPlayer occupied from
        f t = S.fromList $ filter ((==p) . occupyingPlayer occupied) (conns ! t)
    in  isJust (findPathSimple f to from)
