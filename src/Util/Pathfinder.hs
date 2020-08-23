module Util.Pathfinder where

import Control.Monad.State
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set (Set)
import qualified Data.List as L
import qualified Data.Set as Set

-- Stolen from https://hackage.haskell.org/package/astar-0.3.0.0/docs/Data-Graph-AStar.html
--
-- In this case "a" would be "Tile". We could likely do away with the
-- distance function for now.
--
-- findPath :: (Hashable a, Ord a, Ord c, Num c)
-- => (a -> Set a)	    -- The graph we are searching through, given as a function from vertices to their neighbours.
-- -> (a -> a -> c)	    -- Distance function between neighbouring
--                      -- vertices of the graph. This will never be applied to vertices that
--                      -- are not neighbours, so may be undefined on pairs that are not
--                      -- neighbours in the graph.
-- -> (a -> c)	        -- Heuristic distance to the (nearest) goal.
--                      -- This should never overestimate the distance, or else the path found
--                      -- may not be minimal.
-- -> (a -> Bool)	    -- The goal, specified as a boolean predicate on vertices.
-- -> a	                -- The vertex to start searching from.
-- -> Maybe [a]	        -- An optimal path, if any path exists. This excludes the starting vertex.


findPathSimple :: (Ord a)
         => (a -> Set a)     -- Tile -> Neighbors
         -> a                -- end
         -> a                -- start
         -> Maybe [a]
findPathSimple f = findPath f (const . const 1)

-- TODO performance
findPath :: (Ord a, Ord c, Num c)
         => (a -> Set a)     -- Tile -> Neighbors
         -> (a -> a -> c)    -- distance function
         -> a                -- end
         -> a                -- start
         -> Maybe [a]
findPath f distance end start
    = evalState (findPathM (finder f) distance end start) Set.empty

-- TODO sort by distance of cur path to end + distance of start to cur path
-- TODO all a's should be wrapped in the monad
findPathM :: (Monad m, Ord a, Ord c, Num c)
         => (a -> m (Set a)) -- Tile -> Neighbors
         -> (a -> a -> c)    -- distance function
         -> a                -- end
         -> a                -- start
         -> m (Maybe [a])
findPathM f distance end start
    | end == start = return (Just [end])
    | otherwise    = do
            neighbors <- L.sortBy sortf . Set.elems <$> f start
            paths     <- mapM (findPathM f distance end) neighbors
            return ((start:) <$> listToMaybe (catMaybes paths))
        where
            -- finder          = \a -> if a == start then return (Set.empty) else f a
            sortf           = comparing distance end
            -- TODO doesn't make much diff
            -- distancef n end = distance n end + distance start n
            comparing f end = \a b -> compare (f a end) (f b end)

-- simple finder implementation using queue
finder :: (Ord a) => (a -> Set a) -> a -> State (Set a) (Set a)
finder f a = do
    queue <- get
    put (Set.insert a queue)
    if a `Set.member` queue then
        return (Set.empty)
    else
        return (f a)
