{-# LANGUAGE ExistentialQuantification #-}
module GameState (
    GameState (..), 
    DeltaState (..), 
    AbstractEntity (..),
    Entity (..)
    ) where
import Graphics.Rendering.Cairo (Render)
import Data.Unique (Unique)
import KeyState
import Mechanics
import Tile
import Message

data GameState = GameState {
    -- All the entities, including the players
    stateEntities :: [AbstractEntity],
    -- The level map
    stateMap :: TileMap,
    -- A predicate for pressed buttons
    stateKeys :: KeyButton -> Bool
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

-- The type class for players, monsters, items, particles, etc.
class Entity a where
    -- The function that updates an entity (self, state, messages, randomSeed, deltaTime)
    entityUpdate :: a -> GameState -> [Message] -> Int -> Duration -> DeltaState
    -- Returns the current position of the entity (if any)
    -- Entities without a position won't be drawn at all
    entityPosition :: a -> Maybe Position
    -- An entity has a bounding box if it can collide with other entities
    entityBox :: a -> Maybe Dimension
    -- Returns the current graphical representation of the entity
    entityDraw :: a -> Render ()
    -- Should this be drawn on top of items and such?
    entityOnTop :: a -> Bool
    -- Non-hitables do not collide with each other (but can collide with hitables)
    entityHitable :: a -> Bool
    -- The identity of the entity
    entityId :: a -> Unique

-- This is to be able to store different kinds of entities in lists.
-- When you make functions to work at entities, please use 
-- (Entity a) => ... instead of AbstractEntity, as the former
-- is more general (ie. it will also work on concrete entities).
data AbstractEntity = forall a. (Entity a) => AbstractEntity a

-- This enables you to work on abstract entities - it just delegates everything
instance Entity AbstractEntity where
    entityUpdate (AbstractEntity e) = entityUpdate e
    entityPosition (AbstractEntity e) = entityPosition e
    entityBox (AbstractEntity e) = entityBox e
    entityDraw (AbstractEntity e) = entityDraw e
    entityOnTop (AbstractEntity e) = entityOnTop e
    entityHitable (AbstractEntity e) = entityHitable e
    entityId (AbstractEntity e) = entityId e
    
type Message = AbstractMessage AbstractEntity

