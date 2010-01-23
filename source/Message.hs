module Message where
import Damage

data AbstractMessage a
    = MessageCollide a
    | MessageDamage a Damage

