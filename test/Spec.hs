{-
This is a test file for the MiniFrame.hs module.
Run 'cabal test' for running the tests.
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Main where

import System.Exit               (exitFailure)
import Control.Monad             (unless)
import Test.QuickCheck.Test      (quickCheckResult, isSuccess)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Control.Applicative       ((<$>), (<*>))
import MiniFrame

import qualified Data.List as List

instance Arbitrary MiniFrame where
    arbitrary = MiniFrame <$> arbitrary <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------

prop_fromSample :: Bool
prop_fromSample = fromSample == MiniFrame n h rs
  where
    n  = "MiniFrame"
    h  = ["C1","C2","C3","C4"]
    rs = [ ["R1-C1","R1-C2","R1-C3","R1-C4"]
         , ["R2-C1","R2-C2","R2-C3","R2-C4"]
         , ["R3-C1","R3-C2","R3-C3","R3-C4"]
         , ["R4-C1","R4-C2","R4-C3","R4-C4"]
         , ["R5-C1","R5-C2","R5-C3","R5-C4"]
         , ["R6-C1","R6-C2","R6-C3","R6-C4"]
         , ["R7-C1","R7-C2","R7-C3","R7-C4"]
         , ["R8-C1","R8-C2","R8-C3","R8-C4"]
         ]

prop_fromNull :: Bool
prop_fromNull = fromNull == MiniFrame "" [] []

prop_fromRows :: Name -> Header -> [Row] -> Bool
prop_fromRows n h rs
    | h /= List.nub h                          = True  -- Bypass
    | h == List.nub h && mf == fromRows n h rs = True
    | otherwise                                = False
    where
        mf = MiniFrame n h rs

prop_fromColumns :: Name -> Header -> [Row] -> Bool
prop_fromColumns n h rs
    | h /= List.nub h                             = True  -- Bypass
    | h == List.nub h && mf == fromColumns n h rs = True
    | otherwise                                   = False
    where
        mf = MiniFrame n h (List.transpose rs)

prop_nameOf :: MiniFrame -> Bool
prop_nameOf mf@(MiniFrame n _ _) = n == nameOf mf

prop_headerOf :: MiniFrame -> Bool
prop_headerOf mf@(MiniFrame _ h _) = h == headerOf mf

prop_rowsOf :: MiniFrame -> Bool
prop_rowsOf mf@(MiniFrame _ _ rs) = rs == rowsOf mf

prop_rowsNum :: MiniFrame -> Bool
prop_rowsNum mf = length (rowsOf mf) == rowsNum mf

prop_columnsNum :: MiniFrame -> Bool
prop_columnsNum mf = length (columnsOf mf) == columnsNum mf

-- If rowsNum and columnsNum are fine, this should be fine too
prop_entriesNum :: MiniFrame -> Bool
prop_entriesNum mf = rowsNum mf * columnsNum mf == entriesNum mf

prop_prependRow :: MiniFrame -> Row -> Bool
prop_prependRow mf r
    | null r || null (rowsOf mf) || length r /= (length . head . rowsOf) mf = True
    | otherwise                                                             = rowsOf (prependRow r mf) == r : rowsOf mf

prop_prependColumn :: Name -> Column -> MiniFrame -> Bool
prop_prependColumn cn c mf
    | not (null $ rowsOf mf) && length c /= length (rowsOf mf) = True
    | otherwise                                                = cond
      where
        cond  = cond1 && cond2
        nmf   = prependColumn cn c mf
        cond1 = headerOf nmf == cn : headerOf mf
        cond2 = rowsOf   nmf == List.transpose (c : List.transpose (rowsOf mf))

prop_appendRow :: MiniFrame -> Row -> Bool
prop_appendRow mf r
    | null r || null (rowsOf mf) || length r /= (length . head . rowsOf) mf = True
    | otherwise                                                             = rowsOf (appendRow r mf) == rowsOf mf ++ [r]

prop_appendColumn :: Name -> Column -> MiniFrame -> Bool
prop_appendColumn cn c mf
    | not (null $ rowsOf mf) && length c /= length (rowsOf mf) = True
    | otherwise                                                = cond
      where
        cond  = cond1 && cond2
        nmf   = appendColumn cn c mf
        cond1 = headerOf nmf == headerOf mf ++ [cn]
        cond2 = rowsOf   nmf == List.transpose (List.transpose (rowsOf mf) ++ [c])

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let tests = [ quickCheckResult prop_fromSample
                , quickCheckResult prop_fromNull
                , quickCheckResult prop_fromRows
                , quickCheckResult prop_fromColumns
                --
                , quickCheckResult prop_nameOf
                , quickCheckResult prop_headerOf
                , quickCheckResult prop_rowsOf
                --
                , quickCheckResult prop_rowsNum
                , quickCheckResult prop_columnsNum
                , quickCheckResult prop_entriesNum
                --
                , quickCheckResult prop_prependRow
                , quickCheckResult prop_prependColumn
                , quickCheckResult prop_appendRow
                , quickCheckResult prop_appendColumn
                ]

    success <- all isSuccess <$> sequence tests
    unless success exitFailure
