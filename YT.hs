module YT
    (
      Yt (..)
    , rowInsertion
    , mempty
    , (<>)
    ) where

import Control.Lens
import Control.Monad.State.Strict
import Data.Semigroup
import Data.List (intercalate,foldl')

newtype Yt = Yt { yt :: [[Int]] } deriving Eq
type Route = [(Int,Int)]

instance Show Yt where
    show = intercalate "\n" . map (intercalate "\t" . map show) . yt

instance Semigroup Yt where
    (<>) = productTableau

instance Monoid Yt where
    mempty = Yt []
    mappend = (<>)

productTableau :: Yt -> Yt -> Yt
productTableau t =
    foldl' (\y x -> fst $ rowInsertion x y) t . concat . reverse . yt

rowInsertion :: Int -> Yt -> (Yt,Route)
rowInsertion x (Yt tb) = runState (loop 1 x tb) [] & _1 %~ Yt & _2 %~ reverse
    where loop row i [] = modify ((row,1):) >> return [[i]]
          loop row i (xs:xss)
              | null rs = modify ((row,length xs+1):) >> return ((xs++[i]) : xss)
              | otherwise = do
                  modify ((row,length ls+1):)
                  ((ls++(i:tail rs)) :) `liftM` loop (row+1) (head rs) xss
              where (ls,rs) = break (>i) xs
