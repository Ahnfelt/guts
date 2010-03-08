module Message where
import Damage

data Message
    = MessageCollide 
    | MessageDamage Damage

