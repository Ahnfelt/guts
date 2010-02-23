module EntityActor (
    Actor (..), 
    actorNew, 
    actorUpdater, actorIntervals, actorMove, actorTryMove, actorAge,
    EntityActor (..),
    Interval, Intervals, actorIntervalsNew
    ) where
import qualified Data.Map as Map
import Control.Monad
import Data.Unique (Unique)
import GameState
import Mechanics hiding (Interval)
import Collision

newtype Interval = Interval Int deriving (Show, Eq, Ord)
newtype Intervals = Intervals [Double] deriving (Show, Eq, Ord)

data Actor = Actor { 
    actorIntervalMap :: Map.Map Interval (Duration, Duration),
    actorPosition :: Position,
    actorVelocity :: Velocity,
    actorTime :: Duration,
    actorTimeLeft :: Duration,
    actorId :: Unique
} deriving Show

actorNew :: Unique -> Position -> Velocity -> Duration -> Intervals -> Actor
actorNew u p v d is = Actor {
    actorIntervalMap = (let Intervals ds = is in Map.fromList (zip (map Interval [0,1..]) (map (\d -> (d, d)) ds))),
    actorPosition = p,
    actorVelocity = v,
    actorTime = d,
    actorTimeLeft = d,
    actorId = u
}

actorIntervalsNew :: [Duration] -> (Intervals, [Interval])
actorIntervalsNew ds = (Intervals ds, map Interval (take (length ds) [0,1..]))

class EntityActor e where
    actorGet :: e -> Actor
    actorSet :: e -> Actor -> e

actorMove :: (EntityAny e, EntityActor e) => Velocity -> EntityMonad k e ()
actorMove v = do
    t <- timePassed
    e <- self
    let a = actorGet e
    change $ \e -> actorSet e (a { actorPosition = actorPosition a .+ (v .* t) })

actorTryMove :: (EntityAny e, EntityActor e) => Velocity -> EntityMonad k e ()
actorTryMove v = do
    t <- timePassed
    e <- self
    let a@Actor { actorPosition = p1 } = actorGet e
    let p2 = actorPosition a .+ (v .* t)
    -- TODO
    [(q1, q2)] <- walls
    let p = case entityRadius e of 
            Just r -> case segmentCircleCollision q1 q2 p1 p2 r of
                Just (i1, i2) -> i2
                Nothing -> p2        
            Nothing -> p2
    change $ \e -> actorSet e (a { actorPosition = p })

actorIntervals :: (EntityAny e, EntityActor e) => Interval -> Bool -> EntityMonad k e r -> EntityMonad k e [r]
actorIntervals i True f = do
    t <- timePassed
    e <- self
    let a@Actor { actorIntervalMap = is } = actorGet e
    let (l, b) = is Map.! i
    let b' = b + t
    let c = truncate (b' / l)
    let b'' = b' - fromIntegral c * l
    change $ \e -> actorSet e (a { actorIntervalMap = Map.insert i (l, b'') is })
    replicateM c f
actorIntervals i False f = do
    t <- timePassed
    e <- self
    let a@Actor { actorIntervalMap = is } = actorGet e
    let (l, b) = is Map.! i
    let b' = min l b
    change $ \e -> actorSet e (a { actorIntervalMap = Map.insert i (l, b') is })
    return []

actorAge e = 1 - actorTimeLeft (actorGet e) / actorTime (actorGet e)

actorUpdater m = entityUpdater $ \_ -> do
    d <- timePassed
    change $ \e -> let a = actorGet e in actorSet e (a { actorTimeLeft = actorTimeLeft a - d })
    e <- self
    if actorAge e >= 1.0 then vanish else m

