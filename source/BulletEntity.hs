module BulletEntity (bulletNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import GameState
import Mechanics
import Message

data Bullet = Bullet { 
    bulletPosition :: Position,
    bulletVelocity :: Velocity,
    bulletTime :: Double,
    bulletTimeLeft :: Double,
    bulletId :: Unique
} deriving Show

-- (position, velocity, timeToLive)
bulletNew :: Position -> Velocity -> Duration -> (Unique -> AbstractEntity)
bulletNew p v t = \i -> AbstractEntity $ Bullet {
    bulletPosition = p,
    bulletVelocity = v,
    bulletTime = t,
    bulletTimeLeft = t,
    bulletId = i}

instance Entity Bullet where

    entityUpdate e s m r d =
        if bulletTimeLeft e <= 0 then DeltaState { deltaEntities = [], deltaMessages = [], deltaSplatter = Nothing } else
        let v = if age e < 0.5 then 1.0 else 1.75 - (age e * 1.5) in
        let (x', y') = bulletPosition e .+ (bulletVelocity e .* (d * v)) in
        DeltaState { 
            deltaEntities = [const (AbstractEntity (e { 
                bulletPosition = (x', y'),
                bulletTimeLeft = bulletTimeLeft e - d,
                bulletVelocity = if not (null m) then (0, 0) else bulletVelocity e
                }))], 
            deltaMessages = [],
            deltaSplatter = Nothing
        }
    
    entityPosition e = Just (bulletPosition e)

    entityRadius e = Just 5

    entityDraw e = do
        let r = age e * 5
        let cr' = interpolate (age e) 0.00 [(0.30, 0.20), (0.50, 0.50), (0.20, 0.00)]
        let cg' = interpolate (age e) 0.10 [(0.20, 0.00), (0.50, 0.10), (0.20, 0.00)]
        let cb' = interpolate (age e) 0.50 [(0.10, 0.00)]
        setSourceRGB cr' cg' cb'
        setOperator OperatorAdd
        arc 0 0 (8 + r * 2.5) 0 (2 * pi)
        fill
        let cr = interpolate (age e) 0.50 [(0.10, 0.30), (0.75, 0.60), (0.05, 0.00)]
        let cg = interpolate (age e) 0.50 [(0.10, 0.30), (0.75, 0.10), (0.05, 0.00)]
        let cb = interpolate (age e) 0.50 [(0.20, 0.10), (0.80, 0.00)]
        setSourceRGB cr cg cb
        setOperator OperatorAdd
        arc 0 0 (5 + r * 2) 0 (1.8 * pi)
        fill

    entityOnTop _ = True

    entityHitable _ = False

    entityId e = bulletId e

age e = 1 - bulletTimeLeft e / bulletTime e

