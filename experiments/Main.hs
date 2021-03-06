module Main where
import Control.Monad
import System.Time

main :: IO ()
main = do
    t <- getClockTime
    loop t
    where
        loop t@(TOD s p) = do
            putStrLn ("s: "++show s)
            putStrLn ("p: "++show p)
            busyWait 1 0
            t' <- getClockTime
            let ds = diff t t'
            putStrLn ("ds: "++show ds)
            loop t'
    

diff :: ClockTime -> ClockTime -> Double
diff (TOD s1 p1) (TOD s2 p2) =
    let ds = fromIntegral (s2 - s1)
        dp = fromIntegral (p2 - p1)
    in ds + (dp * 10**(-12))
        
    
    

busyWait :: Int -> Integer -> IO ()
busyWait s p = do 
    start <- getClockTime
    let end = addToClockTime (noTimeDiff {tdSec = s, tdPicosec = p}) start
    busyWaitUntil end
    where
        busyWaitUntil end = do
            now <- getClockTime
            when (now < end) $ busyWaitUntil end
            

