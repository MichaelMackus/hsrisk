module Game.Attack (attack) where

import Game.Types
import Util (numerate)

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe

import Data.Map ((!))
import System.Random
import qualified Control.Monad.State as State
import qualified Data.Map as M
import qualified Data.List as L

-- attempt an attack
attack :: Territory -> Territory -> GameRenderer ()
attack from to = do
    conns <- State.gets territoryConnections
    ts    <- State.gets occupiedTerritories
    let (p,  occupiedF) = ts ! from
        (p', occupiedT) = ts ! to
    when (p /= p' && isAdjacent conns from to && occupiedF > 1) $ do
        g <- liftIO newStdGen
        let attackers     = min 3 (occupiedF - 1)
            defenders     = min 2 occupiedT
            -- randomly determine results of attack
            (attackersLost, atkRolls, defRolls, _) = rollAttack attackers defenders g
            attackersRem  = attackers - attackersLost
            attackersWon  = max 0 (min attackers defenders - attackersLost)
            defendersLost = defenders - defendersWon
            defendersWon  = max 0 (defenders - attackersWon)
            defendersRem  = occupiedT - defendersLost
            ts'           = if defendersRem > 0 then
                                 -- defender won
                                 M.insert from (p, occupiedF - attackersLost) $ M.insert to (p', occupiedT - defendersLost) ts
                            else
                                 -- attacker won
                                 M.insert from (p, occupiedF - attackers) $ M.insert to (p, attackersRem) ts
        newMessage ("You rolled " ++ show atkRolls ++ ", defending player rolled " ++ show defRolls)
        newMessage ("You lost " ++ show attackersLost ++ " attackers, defending player lost " ++ show defendersLost ++ " defenders")
        State.modify $ \s -> s { phase = Attack Nothing,
                                 occupiedTerritories = ts' }

-- roll attack for n attackers & defenders
-- first part of result contains attackers who lost
rollAttack :: RandomGen g => Int -> Int -> g -> (Int, [Int], [Int], g)
rollAttack attackers defenders g = let (atkRolls, g')  = rollN attackers g
                                       (defRolls, g'') = rollN defenders g'
                                       l               = compareRolls atkRolls defRolls
                                   in  (length (filter (/=GT) l), atkRolls, defRolls, g'')

-- compare attacker & defender rolls to see how many of each won
compareRolls :: [Int] -> [Int] -> [Ordering]
compareRolls atk def = let atkSorted = reverse (L.sort atk)
                           defSorted = reverse (L.sort def)
                           f   (i,r) = compare r (defSorted !! i)
                       in  map f $ numerate (take (length def) atkSorted)

-- roll 6-sided dice n times
rollN :: RandomGen g => Int -> g -> ([Int], g)
rollN n g = let f r (xs, g) = append xs (r g)
                append xs (x, g') = ((x:xs), g')
            in  foldr f ([], g) $ replicate n (randomR (1,6))
