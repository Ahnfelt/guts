module Entity where
import Control.Monad.State.Lazy

type M e r = State e r

class Class e where
    updateEntity :: M e ()

modify f = do
    e <- get
    e' <- f e
    put $ e'

