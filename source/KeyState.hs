module KeyState (KeyState, KeyButton, newKeyState, keyPress, keyRelease, keyPressed) where
import qualified Data.Set as Set
import qualified Data.Char as Char

data KeyState = KeyState (Set.Set KeyButton)
type KeyButton = String

newKeyState = KeyState $ Set.empty
keyPress s (KeyState k) = KeyState $ Set.insert (map Char.toLower s) k
keyRelease s (KeyState k) = KeyState $ Set.delete (map Char.toLower s) k
keyPressed s (KeyState k) = Set.member (map Char.toLower s) k

