{-# LANGUAGE ExistentialQuantification #-}
module Entity where
import Control.Monad.State.Lazy
import Graphics.Rendering.Cairo (Render)
import Data.Unique
import Message
import World.Tile
import World.Barrier

type M e r = State e r

class Class e where
    updateEntity :: e -> GameState -> Int -> DeltaState

modify f = do
    e <- get
    e' <- f e
    put $ e'

-- This is to be able to store different kinds of entities in lists.
-- When you make functions to work at entities, please use 
-- (Entity.Class a) => ... instead of AbstractEntity, as the former
-- is more general (ie. it will also work on concrete entities).
data AbstractEntity = forall a. (Class a) => AbstractEntity a

instance Class AbstractEntity where
    updateEntity (AbstractEntity e) = updateEntity e

data GameState = GameState {
    -- All the entities, including the players
    stateEntities :: [AbstractEntity],
    -- The level map
    stateMap :: TileMap,
    -- A predicate for pressed buttons
    stateKeys :: String -> Bool,
    -- Collision line segments.
    stateBarriers :: BarrierMap
}

-- This represents the result of updating an entity (changes to the game state)
data DeltaState = DeltaState { 
    -- The entities replacing the updated entity
    deltaEntities :: [Unique -> AbstractEntity],
    -- Messages to send to the entities identified by the IDs
    deltaMessages :: [(Unique, Message)],
    -- A permanent drawing to add to the background image
    deltaSplatter :: Maybe (Render ())
}

deltaStateNew = DeltaState { deltaEntities = [], deltaMessages = [], deltaSplatter = Nothing }

