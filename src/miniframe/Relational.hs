{- |
Module      :  Relational.hs
Description :  Relational algebra operations
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

Some relational algebra goodness on top of minimal data frames.
-}

module MiniFrame.Relational
    ( union      -- MiniFrame -> MiniFrame -> MiniFrame
    , diff       -- MiniFrame -> MiniFrame -> MiniFrame
    , intersect  -- MiniFrame -> MiniFrame -> MiniFrame
    , cartprod   -- MiniFrame -> MiniFrame -> MiniFrame
    , project    -- [Name] -> MiniFrame -> MiniFrame
    , rename     -- Name -> Name -> MiniFrame -> MiniFrame
    ) where

import MiniFrame.Frames
import Optimize

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

-- | Union operation
union :: MiniFrame -> MiniFrame -> MiniFrame
union (MiniFrame n h rs) (MiniFrame on oh ors)
    | h /= oh   = error "Header mismatch"
    | otherwise = MiniFrame nn nh nrs
    where
        nn  = "Union: " ++ n ++ " and " ++ on
        nh  = h
        nrs = rs `listUnion` ors

-- | Difference operation
diff :: MiniFrame -> MiniFrame -> MiniFrame
diff (MiniFrame n h rs) (MiniFrame on oh ors)
    | h /= oh = error "Header mismatch"
    | otherwise = MiniFrame nn nh nrs
    where
        nn  = "Difference: " ++ n ++ " and " ++ on
        nh  = h
        nrs = rs `listDiff` ors

-- | Intersect operation
intersect :: MiniFrame -> MiniFrame -> MiniFrame
intersect (MiniFrame n h rs) (MiniFrame on oh ors)
    | h /= oh   = error "Header mismatch"
    | otherwise = MiniFrame nn nh nrs
    where
        nn  = "Intersection: " ++ n ++ " and " ++ on
        nh  = h
        nrs = rs `listIntersect` ors

-- | Cartesian product operation
cartprod :: MiniFrame -> MiniFrame -> MiniFrame
cartprod (MiniFrame n h rs) (MiniFrame on oh ors)
    | h `listDiff` oh /= h = error "Cannot perform cartesian product on duplicate column names"
    | otherwise            = MiniFrame nn nh nrs
    where
        nn  = n ++ "Cartesian product: " ++ on
        nh  = h ++ oh
        nrs = [x ++ y | x <- rs, y <- ors]

-- | Project operation
project :: [Name] -> MiniFrame -> MiniFrame
project cns mf@(MiniFrame n h rs)
    | not $ all (`elem` h) cns = error "Column name does not exist"
    | otherwise                = MiniFrame nn nh nrs
    where
        nn  = "Projected: " ++ n
        nh  = cns
        nrs = List.transpose $ map (`columnByName` mf) cns

-- | Rename operation
rename :: Name -> Name -> MiniFrame -> MiniFrame
rename ocn ncn mf@(MiniFrame n h rs)
    | ocn `elem` h = MiniFrame nn nh nrs
    | otherwise    = mf
    where
        i   = Maybe.fromJust (List.elemIndex ocn h)
        nn  = n
        nh  = take i h ++ [ncn] ++ drop (i + 1) h
        nrs = rs
