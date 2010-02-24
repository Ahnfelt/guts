module EntityActor (
    Actor (..), 
    actorNew, 
    actorUpdater, actorIntervals, actorMove, actorTryMove, actorMoveTowards, actorAge,
    EntityActor (..),
    Interval, Intervals, actorIntervalsNew
    ) where
import qualified Data.Map as Map
import Data.Ord
import Data.List
import Data.Maybe
import Control.Monad
import Data.Unique (Unique)
import GameState
import Mechanics hiding (Interval)
import Collision
import Barrier
import Debug.Trace


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

tryMove :: Position -> Velocity -> Maybe Double -> BarrierMap -> Maybe (Position, Position, LineSegment)
tryMove p1 v Nothing bm = Nothing
tryMove p1 v (Just r) bm = 
    let p2 = p1 .+ v
        bs = barriersNear bm (p1, p2) r
        stopBeforeWall l@(q1, q2) = 
            do p <- segmentCircleCollision q1 q2 p1 p2 r
               return (p, l)
    in case catMaybes $ map stopBeforeWall bs of
        [] -> Nothing
        ps -> let ((i1, i2), l) = minimumBy (comparing (squaredDistance p1 . snd . fst)) ps
              in Just (i1, i2, l)


actorTryMove :: (EntityAny e, EntityActor e) => Velocity -> (Vector -> EntityMonad k e ()) -> EntityMonad k e ()
actorTryMove v f = do
    t <- timePassed
    e <- self
    let a = actorGet e
    let p1 = actorPosition a
    let v' = v .* t
    let p2 = p1 .+ v'
    bm <- barriers
    let r = entityRadius e
    case tryMove p1 v' r bm of
        Just (i1, i2, l) -> do
            change $ \e -> actorSet e (a { actorPosition = i2 .- (norm v') })
            f i1
        Nothing -> do 
            change $ \e -> actorSet e (a { actorPosition = p2 })

actorMoveTowards :: (EntityAny e, EntityActor e) => Velocity -> EntityMonad k e ()
actorMoveTowards v = do
    t <- timePassed
    e <- self
    let a = actorGet e
    let p1 = actorPosition a
    let v' = v .* t
    let p2 = p1 .+ v'
    bm <- barriers
    let r = entityRadius e
    let p = case tryMove p1 v' r bm of
            Just (i1, i2, (q1, q2)) -> 
                let x = p2 .- i2
                    y = q2 .- q1
                    xy = norm y .* ((x `dot` y) / vectorLength y)
                in (xy .+ i2) .- (norm v')
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
    let b' = min l (b + t)
    change $ \e -> actorSet e (a { actorIntervalMap = Map.insert i (l, b') is })
    return []

actorAge e = 1 - actorTimeLeft (actorGet e) / actorTime (actorGet e)

actorUpdater m = entityUpdater $ \_ -> do
    d <- timePassed
    change $ \e -> let a = actorGet e in actorSet e (a { actorTimeLeft = actorTimeLeft a - d })
    e <- self
    if actorAge e >= 1.0 then vanish else m

