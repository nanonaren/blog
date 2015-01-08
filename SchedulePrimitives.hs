{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SchedulePrimitives where

import Control.Monad
import Control.Monad.State

newtype Schedule2 r a = Schedule2 {runSchedule2 :: StateT (r,r) [] a}
    deriving (Functor)

cover :: (Ord r,Enum r)
      => (r -> (a,r,r)) -- ^ r -> (a,x,y) such that x <= r
      -> Schedule2 r (a,(r,r))
cover p = Schedule2 fetch
  where fetch = do
          (x,y) <- get
          let (res,x',y') = p x
          when (x' > x) $ error "match: x' > x"
          (put (x,min y' y) >> return (res,(x',y'))) `mplus`
            (guard (y' < y) >> put (succ y',y) >> fetch)

range :: (Ord r,Enum r,Bounded r) => r -> r -> Schedule2 r (Bool,(r,r))
range a b
  | a > b = error "range: require a <= b"
  | otherwise = cover $ \x ->
      if x < a
      then (False,minBound,pred a)
      else if x <= b
           then (True,a,b)
           else (False,succ b,maxBound)

periodic :: (Integral r,Show r)
         => r
         -> r
         -> r
         -> Schedule2 r ((Int,Bool),(r,r))
periodic a b next
  | a > b = error "periodic: require a <= b"
  | next <= b = error "periodic: next > b"
  | otherwise = cover $ \x ->
      let n = (x-a) `div` (next-a)
          a' = a + n*(next-a)
          b' = a' + (b-a)
      in if x <= b'
         then ((fromIntegral n,True),a',b')
         else ((fromIntegral n,False),succ b',pred $ b' + (next-b))

runCover :: Schedule2 r a -> (r,r) -> [(a,(r,r))]
runCover m s = runStateT (runSchedule2 m) s
