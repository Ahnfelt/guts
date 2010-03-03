module FlameEntity (spawnFlame) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import Layer
import Damage
import GameState
import Mechanics
import Message
import EntityActor

data Flame = Flame { 
    flameAngle :: Angle,
    flameVelocity :: Velocity,
    flameRotation :: Angle,
    flameRotationSpeed :: Angle,
    flamePassive :: Bool,
    flameMarkInterval :: Interval,
    flameActor :: Actor
} deriving Show

-- (position, velocity, angle, timeToLive)
spawnFlame :: EntityAny e => Position -> Velocity -> Angle -> Duration -> EntityMonad k e ()
spawnFlame p v a t = do
    r1 <- randomDouble
    let (ds, [markInterval]) = actorIntervalsNew [0.1]
    spawn $ \u -> AbstractEntity $ Flame {
        flameAngle = a,
        flameVelocity = v,
        flameRotation = 0,
        flameRotationSpeed = (3 - 6 * r1) * pi,
        flamePassive = False,
        flameMarkInterval = markInterval,
        flameActor = actorNew u p v t ds
    }

instance EntityActor Flame where
    actorGet e = flameActor e
    actorSet e a = e { flameActor = a }

instance EntityAny Flame

instance Entity Flame where

    entityUpdate = actorUpdater $ do
        receive $ do
            MessageCollide <- message
            reply $ MessageDamage $ damageNew { damageBurning = 0.02 }
            change $ \e -> e {
                flameVelocity = (0, 0),
                flamePassive = True
            }
        e <- self
        actorIntervals (flameMarkInterval e) True $ splatter $ do
            setSourceRGBA 0 0 0 0 -- .2
            arc 0 0 8 0 (2 * pi)
            fill
        let v = interpolate (actorAge e) 1.0 [(0.5, 1.0), (0.5, 0.5)]
        actorTryMove (flameVelocity e .* v) $ \p -> return ()
        d <- timePassed
        change $ \e -> e {
            flameRotation = flameRotation e + flameRotationSpeed e * d
        }
    
    entityPosition e = Just (actorPosition $ flameActor e)

    entityRadius e | actorAge e < 0.9 && not (flamePassive e) = Just 1
    entityRadius e = Nothing

    entityDraw e i = do
        when (actorAge e < 0.9) $ do
            save
            let s = interpolate (actorAge e) 0.30 [(0.7, 1.00), (0.2, 0.10), (0.1, 0.01)]
            scale s s
            rotate (flameAngle e)
            setOperator OperatorAdd
            setSourceSurface (i "flame2.png") (-25) (-25)
            paint
            restore
        let s' = interpolate (actorAge e) 0.50 [(0.8, 1.00), (0.2, 0.20)]
        scale s' s'
        rotate (flameRotation e)
        setOperator OperatorAdd
        setSourceSurface (i "flame3.png") (-25) (-25)
        paint

    entityLayer e = LayerProjectile

    entityHitable e = False

    entityId e = actorId (flameActor e)

