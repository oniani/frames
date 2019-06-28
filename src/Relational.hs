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
    , diff       -- Miniframe -> Miniframe -> Miniframe
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

-- | Union operation
union :: Miniframe -> Miniframe -> Miniframe
union (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = "Union: " ++ name ++ " and " ++ otherName
        newHeader = header
        newRows   = rows `List.union` otherRows

-- | Difference operation
diff :: Miniframe -> Miniframe -> Miniframe
diff (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = "Difference: " ++ name ++ " and " ++ otherName
        newHeader = header
        newRows   = rows List.\\ otherRows

-- | Intersect operation
intersect :: Miniframe -> Miniframe -> Miniframe
intersect (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = "Intersection: " ++ name ++ " and " ++ otherName
        newHeader = header
        newRows   = rows `List.intersect` otherRows

-- | Project operation
project :: [Name] -> Miniframe -> Miniframe
project columnNames mf@(Miniframe name header rows)
    | not $ all (`elem` header) columnNames = error "Column name does not exist"
    | otherwise                             = Miniframe newName newHeader newRows
    where
        newName   = "Projected " ++ name
        newHeader = columnNames
        newRows   = List.transpose $ map (columnByName mf) columnNames

-- | Select operation
select :: (Header -> Row -> Bool) -> Miniframe -> Miniframe
select function (Miniframe name header rows) = Miniframe ("Selected " ++ name) header (filter (function header) rows)

-- | Rename operation
rename :: Name -> Name -> Miniframe -> Miniframe
rename oldColumnName newColumnName mf@(Miniframe name header rows)
    | oldColumnName `elem` header = Miniframe newName newHeader newRows
    | otherwise                   = mf
    where
        index     = Maybe.fromJust (List.elemIndex oldColumnName header)
        newName   = name
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header
        newRows   = rows

-- The following is a proto version that needs to be tested

-- | Natural join operation
njoin :: Miniframe -> Miniframe -> Miniframe
njoin mf@(Miniframe name header rows) otherMf@(Miniframe otherName otherHeader otherRows)
    | null commonColumnNames  = error "No common column names"
    | columns                 /= otherColumns = error "No common columns"
    | otherwise               = Miniframe newName newHeader newRows
    where
        commonColumnNames = header `List.intersect` otherHeader
        columns           = map (columnByName mf) commonColumnNames
        otherColumns      = map (columnByName otherMf) commonColumnNames
        ----
        newName           = name ++ " njoin " ++ otherName
        newHeader         = List.nub (header ++ otherHeader)
        newRows           = List.nub (rows ++ otherRows)

-- | Theta join operation
thetaJoin :: (Header -> Row -> Bool) -> Miniframe -> Miniframe -> Miniframe
thetaJoin function miniframe otherMiniframe = select function (njoin miniframe otherMiniframe)

-- | Cartesian product operation
cartprod :: Miniframe -> Miniframe -> Miniframe
cartprod (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header List.\\ otherHeader /= header = error "Cannot perform cartesian product on duplicate column names"
    | otherwise                            = Miniframe newName newHeader newRows
    where
        newName   = name ++ " cartprod " ++ otherName
        newHeader = header ++ otherHeader
        newRows   = [x ++ y | x <- rows, y <- otherRows]
