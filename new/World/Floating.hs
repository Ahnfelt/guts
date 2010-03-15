module World.Floating ((/), acos, sqrt) where
import qualified Prelude as P

-- exception safe operations

unsafe :: P.Double -> P.Bool
unsafe r = P.isInfinite r P.|| P.isNaN r

(/) :: P.Double -> P.Double -> P.Double
a / b = let r = a P./ b in if unsafe r then P.error (P.show a P.++ " / " P.++ P.show b) else r

acos :: P.Double -> P.Double
acos a = let r = P.acos a in if unsafe r then P.error ("acos " P.++ P.show a) else r

sqrt :: P.Double -> P.Double
sqrt a = let r = P.sqrt a in if unsafe r then P.error ("sqrt " P.++ P.show a) else r

