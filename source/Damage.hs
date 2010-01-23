module Damage where

data Damage = Damage {
    piercing :: Double,
    burning :: Double,
    freezing :: Double,
    corrosive :: Double
}

type DamageResistance = Damage

damageHealth :: DamageResistance -> Damage -> Double
damageHealth r d = (
    (1 - piercing r) * piercing d +
    (1 - burning r) * burning d +
    (1 - freezing r) * freezing d + 
    (1 - corrosive r) * corrosive d)

damageResistanceNew = damageNew

damageNew = Damage { 
    piercing = 0,
    burning = 0,
    freezing = 0,
    corrosive = 0 }

