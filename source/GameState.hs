{-# LANGUAGE ExistentialQuantification #-}
module GameState (GameState (..), Update (..), Position, AbstractEntity, Entity (..), Player (..)) where
import Graphics.Rendering.Cairo (Render)
import Tile

data GameState = GameState {
    -- The players in the game
    statePlayers :: [Player],
    -- All the entities, including the players
    stateEntities :: [AbstractEntity],
    -- The level map
    stateMap :: TileMap
}

-- This represents the result of updating an entity
data Update = Update { 
    -- The entities replacing the updated entity
    updateEntities :: [AbstractEntity],
    -- A permanent drawing to add to the background image
    updateSplatter :: Render () 
}

-- The type class for players, monsters, items, particles, etc.
class Entity a where
    -- The function that updates an entity (self, state, deltaTime)
    entityUpdate :: a -> GameState -> Double -> Update
    entityUpdate e _ _ = Update { updateEntities = [entity e], updateSplatter = return () }
    -- Returns the current position of the entity (if any)
    -- Entities without a position won't be drawn at all
    entityPosition :: a -> Maybe Position
    entityPosition _ = Nothing
    -- Returns the current graphical representation of the entity
    entityDraw :: a -> Render ()
    entityDraw _ = return ()
    -- Should this be drawn on top of items and such?
    entityOnTop :: a -> Bool
    entityOnTop _ = True
    -- Converts any entity to an abstract entity (for storage in lists etc.)
    entity :: a -> AbstractEntity
    entity = AbstractEntity

-- This is to be able to store different kinds of entities in lists.
-- When you make functions to work at entities, please use 
-- (Entity a) => ... instead of AbstractEntity, as the former
-- is more general (ie. it will also work on concrete entities).
data AbstractEntity = forall a. (Entity a) => AbstractEntity a

-- This enables you to work on abstract entities
instance Entity AbstractEntity where
    entityUpdate (AbstractEntity e) = entityUpdate e
    entityPosition (AbstractEntity e) = entityPosition e
    entityDraw (AbstractEntity e) = entityDraw e
    entityOnTop (AbstractEntity e) = entityOnTop e
    entity = id -- Avoids needless boxing
    
-- A position (x, y)
type Position = (Double, Double)

-- Needs to be declared here because it's part of GameState
data Player = Player { 
    playerPosition :: Position 
}

