module Entity.Projectile.Pellet (spawnPellet) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import Layer
import GameState
import World.Mechanics
import Message
import Message.Damage
import World.Tile

data Pellet = Pellet { 
    pelletVelocity :: Velocity,
    pelletAngle :: Angle,
    pelletActor :: Actor
} deriving Show

-- (position, velocity, angle, timeToLive)
spawnPellet :: EntityAny e => Position -> Velocity -> Angle -> Duration -> EntityMonad k e ()
spawnPellet p v a t = do
    let (ds, _) = actorIntervalsNew []
    spawn $ \u -> AbstractEntity $ Pellet {
        pelletVelocity = v,
        pelletAngle = a,
        pelletActor = actorNew u p v t ds
    }

instance EntityActor Pellet where
    actorGet e = pelletActor e
    actorSet e a = e { pelletActor = a }

instance EntityAny Pellet

instance Entity Pellet where

    entityUpdate = actorUpdater $ do
        receive $ do
            MessageCollide <- message
            reply $ MessageDamage $ damageNew { damagePiercing = 0.02 }
            vanish
        e <- self
        actorTryMove (pelletVelocity e) $ \p -> vanish
    
    entityPosition e = Just (actorPosition $ pelletActor e)

    entityRadius e = Just 1

    entityDraw e i = do
        rotate (pelletAngle e)
        setLineWidth 2
        setSourceRGB 0.90 0.80 0.4 
        lineTo 8 0
        stroke
        moveTo 4 2
        setSourceRGB 0.9 0.9 0.7
        lineTo 7 0
        stroke

    entityLayer e = LayerProjectile

    entityHitable e = False

    entityId e = actorId (pelletActor e)

