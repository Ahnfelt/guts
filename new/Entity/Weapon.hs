{-# LANGUAGE ExistentialQuantification, PolymorphicComponents #-}
module Entity.Weapon where

import GameState
import Control.Monad
import World.Mechanics


class Weapon a where
    fire :: (EntityAny e, EntityActor e) => a -> Position -> Angle -> EntityMonad k e ()
    reload :: (EntityAny e) => a -> EntityMonad k e ()
    
data AbstractWeapon = forall a. (Show a, Weapon a) => AbstractWeapon a

instance Weapon AbstractWeapon where
    fire (AbstractWeapon w) = fire w
    reload (AbstractWeapon w) = reload w
  
instance Show AbstractWeapon where
    show (AbstractWeapon w) = show w
    

data Firearm = Firearm {
    firearmInterval :: Interval,
    firearmSpread :: Double,
    firearmProjectiles :: Int,
    firearmFire :: (EntityAny e, EntityActor e) => Firearm -> Position -> Angle -> EntityMonad k e ()
}


instance Show Firearm where
    show f = "firearm"

instance Weapon Firearm where
    fire f p a = do
            actorIntervals (firearmInterval f) True $ do
                replicateM_ (firearmProjectiles f) $ firearmFire f f p a
            return ()
    reload w = return ()
