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

module Relational
    ( union      -- Miniframe -> Miniframe -> Miniframe
    , (\\)       -- Miniframe -> Miniframe -> Miniframe
    , intersect  -- Miniframe -> Miniframe -> Miniframe
    , project    -- [Name] -> Miniframe -> Miniframe
    , select     -- (Header -> Row -> Bool) -> Miniframe -> Miniframe
    , rename     -- Name -> Name -> Miniframe -> Miniframe
    , njoin      -- Miniframe -> Miniframe -> Miniframe
    , thetaJoin  -- (Header -> Row -> Bool) -> Miniframe -> Miniframe -> Miniframe
    , cartprod   -- Miniframe -> Miniframe -> Miniframe
    ) where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import Miniframe

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

-- | Union operation from relational algebra
union :: Miniframe -> Miniframe -> Miniframe
union (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = name ++ " union " ++ otherName
        newHeader = header
        newRows   = rows `List.union` otherRows

-- | Difference operation from relational algebra
(\\) :: Miniframe -> Miniframe -> Miniframe
(\\) (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = name ++ " difference " ++ otherName
        newHeader = header
        newRows   = rows List.\\ otherRows

-- | Intersect operation from relational algebra
intersect :: Miniframe -> Miniframe -> Miniframe
intersect (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header
        newRows   = rows `List.intersect` otherRows

-- Note that this won't work if the user does not pass the list of columns in the right order.
-- This is due to how `List.isSubsequenceOf` works. Will have to reimplement this operation so that
-- the order does not play any role in determining the output.

-- | Project operation from relational algebra
project :: [Name] -> Miniframe -> Miniframe
project columnNames miniframe@(Miniframe name header rows)
    | columnNames `List.isSubsequenceOf` header = Miniframe newName newHeader newRows
    | otherwise                                 = error "Header mismatch: put the columns in the right order!"
    where
        newName   = "Projected " ++ name
        newHeader = columnNames
        newRows   = List.transpose (map (columnByName miniframe) columnNames)

-- | Select operation from relational algebra
select :: (Header -> Row -> Bool) -> Miniframe -> Miniframe
select function (Miniframe name header rows) = Miniframe ("Selected " ++ name) header (filter (function header) rows)

-- | Rename operation from relational algebra
rename :: Name -> Name -> Miniframe -> Miniframe
rename oldColumnName newColumnName miniframe@(Miniframe name header rows)
    | oldColumnName `elem` header = Miniframe newName newHeader newRows
    | otherwise                   = miniframe
    where
        index     = Maybe.fromJust (List.elemIndex oldColumnName header)
        newName   = name
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header
        newRows   = rows


-- The following is a proto version that needs to be tested

-- | Natural join operation from relational algebra
njoin :: Miniframe -> Miniframe -> Miniframe
njoin miniframe@(Miniframe name header rows) otherMiniframe@(Miniframe otherName otherHeader otherRows)
    | null commonColumnNames  = error "No common column names"
    | columns                 /= otherColumns = error "No common columns"
    | otherwise               = Miniframe newName newHeader newRows
    where
        commonColumnNames = header `List.intersect` otherHeader
        columns           = map (columnByName miniframe) commonColumnNames
        otherColumns      = map (columnByName otherMiniframe) commonColumnNames
        ----
        newName           = name ++ " njoin " ++ otherName
        newHeader         = List.nub (header ++ otherHeader)
        newRows           = List.nub (rows ++ otherRows)

-- | Theta join operation from relational algebra
thetaJoin :: (Header -> Row -> Bool) -> Miniframe -> Miniframe -> Miniframe
thetaJoin function miniframe otherMiniframe = select function (njoin miniframe otherMiniframe)

-- | Cartesian product operation from relational algebra
cartprod :: Miniframe -> Miniframe -> Miniframe
cartprod (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header List.\\ otherHeader /= header = error "Cannot perform cartesian product on duplicate column names"
    | otherwise                            = Miniframe newName newHeader newRows
    where
        newName   = name ++ " cartprod " ++ otherName
        newHeader = header ++ otherHeader
        newRows   = [x ++ y | x <- rows, y <- otherRows]
