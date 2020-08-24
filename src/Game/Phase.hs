module Game.Phase where

import Game.Attack
import Game.Types
import Util

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, fromMaybe)
import qualified Control.Monad.State as State
import qualified Data.Map as M

changePhase :: Phase -> GameRenderer ()
changePhase p = do
    State.modify (\s -> s { phase = p })
    case p of
        (Assign n) -> newMessage ("You get " ++ show n ++ " units! Assign them to your territories." )
        (Attack _) -> newMessage ("Attack phase - choose territory to attack from, then choose a target. Press ENTER when done.")
        (Move   _) -> newMessage ("Move phase - choose territory to move from, then choose a target. Press ENTER when done.")

advanceTurn :: GameRenderer ()
advanceTurn = do
    phase <- State.gets phase
    case phase of
        (Assign n) -> changePhase (Attack Nothing)
        (Attack _) -> do
            --  TODO end turn if nothing to move
            -- p <- return . fromJust =<< State.gets playing
            -- let f t (p',cnt) ts = if p == p' && cnt > 1 then True
            --                       else False
            -- ts <- playerTerritories
            -- ts <- return . M.foldrWithKey f [] =<< State.gets occupiedTerritories
            changePhase (Move Nothing)
        (Move   _) -> do
            {-- advance to next turn! --}
            p  <- State.gets playing
            ps <- State.gets players
            let p' = fromMaybe (error "Unable to find next player") $ next p (filter isHuman ps)
            newMessage ("It is now player " ++ show (playerNum p') ++ "'s turn")
            State.modify (\s -> s { playing = Just p' })
            changePhase . Assign =<< assignableUnits


interactWithTerritory :: Player -> Territory -> Phase -> GameRenderer ()
interactWithTerritory p t (Assign n) = do
                ts <- State.gets occupiedTerritories
                let (p', occupied) = fromJust (M.lookup t ts)
                when (p == p') $ do
                    let occupied' = M.insert t (p, occupied + 1) ts
                    State.modify $ \s -> s {
                        phase = (Assign (n - 1)),
                        occupiedTerritories = occupied'
                    }
                    when (n /= 1) . liftIO $ putStrLn ("You have " ++ show (n - 1) ++ " units left")
                when (n == 1) advanceTurn
interactWithTerritory p t (Attack Nothing) = do
                ts <- State.gets occupiedTerritories
                let (p', occupied) = fromJust (M.lookup t ts)
                when (p == p' && occupied > 1) $ do
                    newMessage "Choose an adjacent territory to attack... (ESCAPE aborts)"
                    State.modify $ \s -> s { phase = (Attack (Just t)) }
interactWithTerritory p t (Attack (Just from)) = attack from t
interactWithTerritory p t (Move   Nothing) = do
                ts <- State.gets occupiedTerritories
                let (p', occupied) = fromJust (M.lookup t ts)
                when (occupied > 1 && p == p') $ do
                    newMessage "Choose a connecting territory to move to... (ESCAPE aborts)"
                    State.modify $ \s -> s { phase = (Move (Just t)) }
interactWithTerritory p t (Move   (Just from)) = do
                conns <- State.gets territoryConnections
                ts    <- State.gets occupiedTerritories
                let (_, occupied)  = fromJust (M.lookup from ts)
                    (_, occupied') = fromJust (M.lookup t ts)
                when (occupied > 1 && from /= t && isConnected conns ts from t) $ do
                    newMessage "Moving 1 unit to connecting territory, click again to move another... (ESCAPE aborts)"
                    let ts' = M.insert from (p, occupied - 1) $ M.insert t (p, occupied' + 1) ts
                        newPhase = if occupied > 2 then Move (Just from) else Move Nothing
                    State.modify $ \s -> s { occupiedTerritories = ts', phase = newPhase }
                    when (newPhase /= Move (Just from)) $ newMessage "Done moving, choose another territory to move from. Press ENTER when done."
