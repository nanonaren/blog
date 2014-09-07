{-# LANGUAGE TemplateHaskell #-}
module YTTest
    (
    ) where

import YT
import Data.List (foldl')
import Test.QuickCheck

number :: Gen Int
number = choose (1,30)

instance Arbitrary Yt where
    arbitrary = fmap (foldl' f mempty) $ vectorOf 20 number
        where f tb x = fst $ rowInsertion x tb

prop_associative_schensted :: Property
prop_associative_schensted = do
  forAll (arbitrary::Gen Yt) $ \y1 ->
    forAll arbitrary $ \y2 ->
      forAll arbitrary $ \y3 -> y1 <> (y2 <> y3) == (y1 <> y2) <> y3

prop_row_bumping_lemma :: Property
prop_row_bumping_lemma = forAll arbitrary $ \yt -> do
  x <- number
  x' <- number
  let (yt1,r1) = rowInsertion x yt
      (_,r2)   = rowInsertion x' yt1
      (row1,col1) = last r1
      (row2,col2) = last r2
  return $ if x <= x'
           then cmp r1 r2 == LT && col1 < col2 && row1 >= row2
           else cmp r2 r1 /= GT && col2 <= col1 && row2 > row1
    where cmp xs ys = let l = min (length xs) (length ys)
                      in compare (take l xs) (take l ys)

return []
runTests = $quickCheckAll
