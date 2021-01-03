{- |
Module      :  Optimize.hs
Description :  This is a description of the module
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

Optimizations.
Confer https://github.com/nh2/haskell-ordnub
-}

module Optimize
    ( ordNub
    , listUnion
    , listDiff
    , listIntersect
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- O(n log n)
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

listUnion :: (Ord a) => [a] -> [a] -> [a]
listUnion a b = a ++ ordNub (filter (`Set.notMember` aSet) b)
  where
    aSet = Set.fromList a

listDiff :: (Ord a) => [a] -> [a] -> [a]
listDiff a b = go initHist a
  where
    initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]

    go _    []     = []
    go hist (x:xs) = case Map.lookup x hist of
      Just n | n > 0 ->     go (Map.insert x (n-1) hist) xs
      _              -> x : go hist                      xs

listIntersect :: (Ord a) => [a] -> [a] -> [a]
listIntersect a b = filter (`Set.member` bSet) a
  where
    bSet = Set.fromList b
