module Entity.Weapon.Shotgun where

import GameState
import System.Random
import World.Mechanics
import Entity.Weapon
import Entity.Projectile.Pellet


newShotgun :: AbstractWeapon
newShotgun = 
    AbstractWeapon $ Firearm {
        firearmInterval = (head . snd) (actorIntervalsNew [0.08]),
        firearmSpread = 0.4,
        firearmProjectiles = 100,
        firearmFire = fire
    }
    where 
        fire :: (EntityAny e) => Firearm -> Position -> Angle -> EntityMonad k e ()
        fire firearm position angle = do
            let spread = firearmSpread firearm
            spreadRandom <- randomDouble
            speedRandom <- randomDouble
            timeToLiveRandom <- randomDouble
            let spreadRandom' = 1 - (log(1 / spreadRandom - 1) + 6) / 12
            let angle' = angle - 0.5 * spread + spreadRandom' * spread
            spawnPellet
                (position .+ velocity angle' 20)
                (velocity angle' (600 + speedRandom * 150))
                angle'
                (0.3 + timeToLiveRandom * 0.3)
            
