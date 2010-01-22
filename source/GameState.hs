{-# LANGUAGE ExistentialQuantification #-}
module GameState (
    GameState (..), 
    DeltaState (..), 
    AbstractEntity, 
    Entity (..),
    EntityId, entityIdNew, entityIdDefault,
    Message
    ) where
import Graphics.Rendering.Cairo (Render)
import KeyState
import Mechanics
import Tile
import Message

data GameState = GameState {
    -- All the entities, including the players
    stateEntities :: [AbstractEntity],
    -- The level map
    stateMap :: TileMap,
    -- The input buttons pressed
    stateKeys :: KeyState
}

-- This represents the result of updating an entity (changes to the game state)
data DeltaState = DeltaState { 
    -- The entities replacing the updated entity
    deltaEntities :: [AbstractEntity],
    -- Messages to send to the entities identified by the IDs
    deltaMessages :: [(EntityId, Message)],
    -- A permanent drawing to add to the background image
    deltaSplatter :: Maybe (Render ())
}

-- The type class for players, monsters, items, particles, etc.
class Entity a where
    -- The function that updates an entity (self, state, deltaTime)
    entityUpdate :: a -> GameState -> [Message] -> Double -> DeltaState
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
    entityId :: a -> EntityId
    -- The identity of the entity
    entityChangeId :: a -> EntityId -> AbstractEntity
    -- Converts any entity to an abstract entity (for storage in lists etc.)
    entity :: a -> AbstractEntity
    entity e = AbstractEntity e

-- This is to be able to store different kinds of entities in lists.
-- When you make functions to work at entities, please use 
-- (Entity a) => ... instead of AbstractEntity, as the former
-- is more general (ie. it will also work on concrete entities).
data AbstractEntity = forall a. (Entity a) => AbstractEntity a

-- This enables you to work on abstract entities
instance Entity AbstractEntity where
    entityUpdate (AbstractEntity e) = entityUpdate e
    entityPosition (AbstractEntity e) = entityPosition e
    entityBox (AbstractEntity e) = entityBox e
    entityDraw (AbstractEntity e) = entityDraw e
    entityOnTop (AbstractEntity e) = entityOnTop e
    entityHitable (AbstractEntity e) = entityHitable e
    entityId (AbstractEntity e) = entityId e
    entityChangeId (AbstractEntity e) = entityChangeId e
    entity e = e -- Avoids needless boxing
    
newtype EntityId = EntityId Integer deriving (Eq, Ord)

entityIdNew = EntityId (-1)
entityIdDefault (EntityId (-1)) i = (EntityId i)
entityIdDefault i _ = i

type Message = AbstractMessage AbstractEntity

