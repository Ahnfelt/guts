module Message where
import Message.Damage

data Message
    = MessageCollide 
    | MessageDamage Damage

