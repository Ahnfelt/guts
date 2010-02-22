{-# LANGUAGE ExistentialQuantification #-}
module GameState (
    GameState (..), 
    DeltaState (..), deltaStateNew,
    AbstractEntity (..),
    Entity (..),
    EntityMonad, EntityAny (..), entityUpdater
    ) where
import Graphics.Rendering.Cairo (Render, Surface)
import System.Random
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Error
import Data.Either
import Data.List
import Data.Unique (Unique)
import KeyState
import Mechanics
import Tile
import Layer
import Message

data GameState = GameState {
    -- All the entities, including the players
    stateEntities :: [AbstractEntity],
    -- The level map
    stateMap :: TileMap,
    -- A predicate for pressed buttons
    stateKeys :: KeyButton -> Bool
}

{-
-- Possible notation:
receive $ do
    MessageCollide <- message
    reply $ MessageDamage $ damageNew { damageBurning = 0.02 }
-}

-- This represents the result of updating an entity (changes to the game state)
data DeltaState = DeltaState { 
    -- The entities replacing the updated entity
    deltaEntities :: [Unique -> AbstractEntity],
    -- The entity itself
    deltaSelf :: AbstractEntity,
    -- Messages to send to the entities identified by the IDs
    deltaMessages :: [(Unique, Message)],
    -- A permanent drawing to add to the background image
    deltaSplatter :: Maybe (Render ())
}

deltaStateNew e = DeltaState { deltaEntities = [], deltaSelf = e, deltaMessages = [], deltaSplatter = Nothing }

-- (extraState, entity, returned)
type EntityMonad k e a = ErrorT String (State (k, EntityData e)) a

data (Entity e) => EntityData e = EntityData {
    dataMessages :: [(AbstractEntity, Message)],
    dataState :: GameState,
    dataDelta :: DeltaState,
    dataPassed :: Duration,
    dataRandom :: StdGen,
    dataKeepAlive :: Bool,
    dataSelf :: e
}

newEntityData s m r d e = EntityData {
    dataMessages = m,
    dataState = s,
    dataDelta = deltaStateNew undefined,
    dataPassed = d,
    dataRandom = r,
    dataKeepAlive = True,
    dataSelf = e
}

executeEntityMonad :: (Entity e) => EntityData e -> EntityMonad () e a -> DeltaState
executeEntityMonad d body = let (Right _, (_, d')) = runState (runErrorT body) ((), d) in 
    (if dataKeepAlive d' 
        then (dataDelta d') { 
            deltaEntities = const (AbstractEntity (dataSelf d')) : deltaEntities (dataDelta d')
        } 
        else dataDelta d') { deltaSelf = AbstractEntity (dataSelf d') }

class Entity e => EntityAny e where
    self :: EntityMonad k e e
    self = do
        (_, a) <- get
        return (dataSelf a)

    change :: (e -> e) -> EntityMonad k e ()
    change f = do
        e <- self
        (k, a) <- get
        put (k, a { dataSelf = f e })

    spawn :: (Unique -> AbstractEntity) -> EntityMonad k e ()
    spawn e' = do
        (k, a@EntityData { dataDelta = d@DeltaState { deltaEntities = l } }) <- get
        put (k, a { dataDelta = d { deltaEntities = e' : l } })

    vanish :: EntityMonad k e ()
    vanish = do
        (k, a) <- get
        put (k, a { dataKeepAlive = False })

    send :: (Entity e') => e' -> Message -> EntityMonad k e ()
    send e' m = do
        (k, a@EntityData { dataDelta = d@DeltaState { deltaMessages = l } }) <- get
        put (k, a { dataDelta = d { deltaMessages = (entityId e', m) : l } })

    sender :: EntityMonad (AbstractEntity, Message) e AbstractEntity
    sender = do
        ((e', _), _) <- get
        return e'

    message :: EntityMonad (AbstractEntity, Message) e Message
    message = do
        ((_, m), _) <- get
        return m

    reply :: Message -> EntityMonad (AbstractEntity, Message) e ()
    reply m = do
        e' <- sender
        send e' m

    receive :: EntityMonad (AbstractEntity, Message) e a -> EntityMonad k e [a]
    receive body = do
        (k, d@EntityData { dataMessages = messages }) <- get
        let (d', l) = mapAccumL receiver d messages
        put (k, d')
        return (rights l)
        where
            receiver d m = 
                let (r, (_, d')) = runState (runErrorT body) (m, d) in
                (d', r)

    splatter :: (Render ()) -> EntityMonad k e ()
    splatter s' = do
        (k, a@EntityData { dataDelta = d@DeltaState { deltaSplatter = s } }) <- get
        let s'' = case s of
                Just s -> do s; s'
                Nothing -> s'
        put (k, a { dataDelta = d { deltaSplatter = Just s'' } })

    randomDouble :: EntityMonad k e Double
    randomDouble = do
        (k, a) <- get
        let (v, r') = random (dataRandom a)
        put (k, a { dataRandom = r' })
        return v

    pressed :: KeyButton -> EntityMonad k e Bool
    pressed b = do
        (_, EntityData { dataState = GameState { stateKeys = f } }) <- get
        return (f b)
        
    timePassed :: EntityMonad k e Duration
    timePassed = do
        (_, a) <- get
        return (dataPassed a)

-- The type class for players, monsters, items, particles, etc.
class (Show a) => Entity a where
    -- The function that updates an entity (self, state, messages, randomSeed, deltaTime)
    entityUpdate :: a -> GameState -> [(AbstractEntity, Message)] -> Int -> Duration -> DeltaState
    -- Returns the current position of the entity (if any)
    -- Entities without a position won't be drawn and won't collide
    entityPosition :: a -> Maybe Position
    -- An only collides with other entities if it has a bounding circle
    entityRadius :: a -> Maybe Double
    -- Returns the current graphical representation of the entity
    entityDraw :: a -> (String -> Surface) -> Render ()
    -- Should this be drawn on top of items and such?
    entityLayer :: a -> Layer
    -- Non-hitables do not collide with each other (but can collide with hitables)
    entityHitable :: a -> Bool
    -- The identity of the entity
    entityId :: a -> Unique

-- Perform all the monad bookkeeping (entityUpdate = entityUpdater $ do ...)
entityUpdater f e s m r d = executeEntityMonad (newEntityData s m (mkStdGen r) d e) (f d)

-- This is to be able to store different kinds of entities in lists.
-- When you make functions to work at entities, please use 
-- (Entity a) => ... instead of AbstractEntity, as the former
-- is more general (ie. it will also work on concrete entities).
data AbstractEntity = forall a. (Show a, Entity a) => AbstractEntity a

-- This enables you to work on abstract entities - it just delegates everything
instance Entity AbstractEntity where
    entityUpdate (AbstractEntity e) = entityUpdate e
    entityPosition (AbstractEntity e) = entityPosition e
    entityRadius (AbstractEntity e) = entityRadius e
    entityHitable (AbstractEntity e) = entityHitable e
    entityDraw (AbstractEntity e) = entityDraw e
    entityLayer (AbstractEntity e) = entityLayer e
    entityId (AbstractEntity e) = entityId e

instance Show AbstractEntity where
    show (AbstractEntity e) = show e

instance Show GameState where
    show s = show (stateEntities s)

instance Show Unique where
    show i = "#"

