module Collidable (Data, Class (..), before, after) where
import Entity (M)

data Data = Data {
    radius :: Float
}

class Class e where
    modify :: (Data -> M e Data) -> M e ()

before :: (Class e) => M e ()
before = return ()

after :: (Class e) => M e ()
after = return ()

