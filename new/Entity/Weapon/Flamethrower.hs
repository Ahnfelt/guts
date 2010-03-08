module Flamethrower where

import Weapon
import EntityActor
import Mechanics
import GameState
import Control.Monad
import System.Random
import FlameEntity


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

