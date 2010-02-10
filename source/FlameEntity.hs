module FlameEntity (flameNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import Layer
import Damage
import GameState
import Mechanics
import Message

data Flame = Flame { 
    flamePosition :: Position,
    flameVelocity :: Velocity,
    flameTime :: Duration,
    flameTimeLeft :: Duration,
    flameAngle :: Angle,
    flameRotation :: Angle,
    flameRotationSpeed :: Angle,
    flamePassive :: Bool,
    flameId :: Unique
} deriving Show

-- (position, velocity, angle, timeToLive, seed)
flameNew :: Position -> Velocity -> Angle -> Duration -> Int -> (Unique -> AbstractEntity)
flameNew p v a t r = let r1:_ = randoms (mkStdGen r) in \i -> AbstractEntity $ Flame {
    flamePosition = p,
    flameVelocity = v,
    flameTime = t,
    flameTimeLeft = t,
    flameAngle = a,
    flameRotation = 0,
    flameRotationSpeed = (3 - 6 * r1) * pi,
    flamePassive = False,
    flameId = i}

instance EntityAny Flame

instance Entity Flame where

    entityUpdate e s m r d | flameTimeLeft e <= 0 = deltaStateNew (AbstractEntity e)
    entityUpdate e s m r d = executeEntityMonad (newEntityData s m (mkStdGen r) e) $ do
        receive $ do
            MessageCollide <- message
            reply $ MessageDamage $ damageNew { damageBurning = 0.02 }
            change $ \e -> e {
                flameVelocity = (0, 0),
                flamePassive = True
            }
        e <- self
        let v = interpolate (age e) 1.0 [(0.5, 1.0), (0.5, 0.5)]
        let (x', y') = flamePosition e .+ (flameVelocity e .* (d * v))
        change $ \e -> e {
            flamePosition = (x', y'),
            flameTimeLeft = flameTimeLeft e - d,
            flameRotation = flameRotation e + flameRotationSpeed e * d
        }
    
    entityPosition e = Just (flamePosition e)

    entityRadius e | age e < 0.9 && not (flamePassive e) = Just 1
    entityRadius e = Nothing

    entityDraw e i = do
        when (age e < 0.9) $ do
            save
            let s = interpolate (age e) 0.30 [(0.7, 1.00), (0.2, 0.10), (0.1, 0.01)]
            scale s s
            rotate (flameAngle e)
            setOperator OperatorAdd
            setSourceSurface (i "flame2.png") (-25) (-25)
            paint
            restore
        let s' = interpolate (age e) 0.50 [(0.8, 1.00), (0.2, 0.20)]
        scale s' s'
        rotate (flameRotation e)
        setOperator OperatorAdd
        setSourceSurface (i "flame3.png") (-25) (-25)
        paint

    entityLayer e = LayerProjectile

    entityHitable e = False

    entityId e = flameId e

-- Calculates the age as a fraction of the total lifetime (0: youngest, 1: oldest)
age e = 1 - flameTimeLeft e / flameTime e

