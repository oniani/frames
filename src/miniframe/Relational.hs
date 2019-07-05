{- |
Module      :  Relational.hs
Description :  Relational algebra operations
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

Some relational algebra goodness on top of minimal data frames.
-}

module MiniFrame.Relational
    ( union      -- MiniFrame -> MiniFrame -> MiniFrame
    , diff       -- MiniFrame -> MiniFrame -> MiniFrame
    , intersect  -- MiniFrame -> MiniFrame -> MiniFrame
    , project    -- [Name] -> MiniFrame -> MiniFrame
    , select     -- (Header -> Row -> Bool) -> MiniFrame -> MiniFrame
    , rename     -- Name -> Name -> MiniFrame -> MiniFrame
    , njoin      -- MiniFrame -> MiniFrame -> MiniFrame
    , thetaJoin  -- (Header -> Row -> Bool) -> MiniFrame -> MiniFrame -> MiniFrame
    , cartprod   -- MiniFrame -> MiniFrame -> MiniFrame
    ) where

import MiniFrame.Frames
import Optimize

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

-------------------------------------------------------------------------------
--                         Relational algebra
--
-- The operators include union, difference, intersect, project, select,
-- rename,and join. These implementations could be improved in various
-- ways (more efficient, flexible etc). Operations like natural join,
-- theta join, equijoin, semijoin, antijoin, division, and cartesian
-- product will be implemented in the next few updates. Some of the
-- operations such as left and right outer joins, left and right inner
-- joins, full outer and full inner joins are yet to be implemented.
-------------------------------------------------------------------------------

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

-- | Project operation
project :: [Name] -> MiniFrame -> MiniFrame
project cns mf@(MiniFrame n h rs)
    | not $ all (`elem` h) cns = error "Column name does not exist"
    | otherwise                = MiniFrame nn nh nrs
    where
        nn  = "Projected " ++ n
        nh  = cns
        nrs = List.transpose $ map (`columnByName` mf) cns

-- | Select operation (it does the partial application and then filters the rows with the rest)
select :: (Header -> Row -> Bool) -> MiniFrame -> MiniFrame
select p (MiniFrame n h rs) = MiniFrame ("Selected: " ++ n) h (filter (p h) rs)

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

-- The following is a proto version that needs to be tested

-- | Natural join operation
njoin :: MiniFrame -> MiniFrame -> MiniFrame
njoin mf@(MiniFrame n h rs) omf@(MiniFrame on oh ors)
    | null ccns  = error "No common column names"
    | cs /= ocs  = error "No common columns"
    | otherwise  = MiniFrame nn nh nrs
    where
        ccns = h `listIntersect` oh
        cs   = map (`columnByName` mf) ccns
        ocs  = map (`columnByName` omf) ccns
        ----
        nn  = n ++ " njoin " ++ on
        nh  = List.nub (h ++ oh)
        nrs = List.nub (rs ++ ors)

-- | Theta join operation
thetaJoin :: (Header -> Row -> Bool) -> MiniFrame -> MiniFrame -> MiniFrame
thetaJoin f mf omf = select f (njoin mf omf)

-- | Cartesian product operation
cartprod :: MiniFrame -> MiniFrame -> MiniFrame
cartprod (MiniFrame n h rs) (MiniFrame on oh ors)
    | h `listDiff` oh /= h = error "Cannot perform cartesian product on duplicate column names"
    | otherwise            = MiniFrame nn nh nrs
    where
        nn  = n ++ " cartprod " ++ on
        nh  = h ++ oh
        nrs = [x ++ y | x <- rs, y <- ors]
