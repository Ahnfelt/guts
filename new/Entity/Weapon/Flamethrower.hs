module Entity.Weapon.Flamethrower where

import World.Mechanics
import GameState
import Control.Monad
import System.Random
import Entity.Weapon
import Entity.Projectile.Flame


newFlamethrower :: AbstractWeapon
newFlamethrower = 
    AbstractWeapon $ Firearm {
        firearmInterval = (head . snd) (actorIntervalsNew [0.03]),
        firearmSpread = 0.3,
        firearmProjectiles = 1,
        firearmFire = fire
    }
    where 
        fire :: (EntityAny e) => Firearm -> Position -> Angle -> EntityMonad k e ()
        fire firearm position angle = do
            let spread = firearmSpread firearm
            positionRandom <- randomDouble
            spreadRandom <- randomDouble
            velocityRandom <- randomDouble
            spawnFlame
                (position .+ velocity angle (20 + 5 * positionRandom))
                (velocity (angle - 0.5 * spread + spreadRandom * spread) (100 + velocityRandom * 50) .* 1.5)
                angle
                1.0

