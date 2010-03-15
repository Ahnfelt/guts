module Message.Damage where

data Damage = Damage {
    damagePiercing :: Double,
    damageBurning :: Double,
    damageFreezing :: Double,
    damageCorrosive :: Double
}

type DamageResistance = Damage

damageHealth :: DamageResistance -> Damage -> Double
damageHealth r d = (
    (1 - damagePiercing r) * damagePiercing d +
    (1 - damageBurning r) * damageBurning d +
    (1 - damageFreezing r) * damageFreezing d + 
    (1 - damageCorrosive r) * damageCorrosive d)

damageResistanceNew = damageNew

damageNew = Damage { 
    damagePiercing = 0,
    damageBurning = 0,
    damageFreezing = 0,
    damageCorrosive = 0 }

