{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module SchedulePrimitives where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Semigroup

-- This can really be compose
-- has the same functor,applicative,alternative instances
newtype Schedule r a = Schedule {runSchedule :: (r,r) -> [(a,(r,r))]}

instance Functor (Schedule r) where
  fmap f (Schedule g) = Schedule $ \r -> map (\(a,r) -> (f a,r)) (g r)

instance Applicative (Schedule r) where
  pure a = Schedule $ \r -> [(a,r)]
  (Schedule f) <*> (Schedule g) = Schedule $ \r -> do
    (func,r') <- f r
    (a,r'') <- g r'
    return (func a,r'')

instance Monad (Schedule r) where
  return = pure
  (Schedule f) >>= g = Schedule $ \r -> do
    (a,r') <- f r
    runSchedule (g a) r'

instance Semigroup m => Semigroup (Schedule r m) where
  s1 <> s2 = (<>) <$> s1 <*> s2

instance (Semigroup m,Monoid m) => Monoid (Schedule r m) where
  mappend = (<>)
  mempty = pure mempty

{-groupBy :: (a -> Bool) -> Schedule r a -> Schedule r (Bool,[a])
groupBy f (Schedule g) = Schedule $ \rr -> loop (g rr)
  where loop [] = []
        loop (x:xs) = loop' (f a,rr) xs
        loop' (b,rr) [] = [(b,rr)]
        loop' (b,(r,r')) ((a,(s,s')),xs)
          | f a == b  = loop' (b,(r,s')) xs
          | otherwise = -}

cover :: (Ord r,Enum r)
      => (r -> (a,r,r)) -- ^ r -> (a,x,y) such that x <= r
      -> Schedule r (a,(r,r))
cover p = Schedule fetch
  where fetch = \(x,y) ->
          let (res,x',y') = p x
          in if (x' > x)
             then error "match: x' > x"
             else ((res,(x',y')),(x,min y' y)) :
                  if y' < y then fetch (succ y',y) else []

single :: (Integral r,Bounded r) => r -> r -> Schedule r (Bool,(r,r))
single a b
  | a > b = error "range: require a <= b"
  | otherwise = cover $ \x ->
      if x < a
      then (False,minBound,pred a)
      else if x <= b
           then (True,a,b)
           else (False,succ b,maxBound)

periodic :: Integral r
         => r
         -> r
         -> r
         -> Schedule r ((Int,Bool),(r,r))
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

pretty :: (Show a,Show r) => [(a,(r,r))] -> IO ()
pretty xs = do
  mapM_ (\(v,t) -> putStrLn $ show t ++ " ---> " ++ show v) xs

getRange :: Schedule r (r,r)
getRange = Schedule $ \rr -> [(rr,rr)]

------------------------------------------------------------
