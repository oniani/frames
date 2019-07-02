{-
This is a test file for the MiniFrame.hs module.
Run 'cabal test' for running the tests.
-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}
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

import MiniFrame

import qualified Data.List as List

instance Arbitrary MiniFrame where
    arbitrary = do  a <- arbitrary
                    b <- arbitrary
                    c <- arbitrary
                    return $ MiniFrame a b c

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
prop_fromRows tn th trs
    | th /= List.nub th                             = True  -- Bypass
    | th == List.nub th && mf == fromRows tn th trs = True
    | otherwise                                     = False
    where
        mf = MiniFrame tn th trs

prop_fromColumns :: Name -> Header -> [Row] -> Bool
prop_fromColumns tn th trs
    | th /= List.nub th                                = True  -- Bypass
    | th == List.nub th && mf == fromColumns tn th trs = True
    | otherwise                                        = False
    where
        mf = MiniFrame tn th (List.transpose trs)

prop_nameOf :: MiniFrame -> Bool
prop_nameOf tmf@(MiniFrame tn _ _) = tn == nameOf tmf

prop_headerOf :: MiniFrame -> Bool
prop_headerOf tmf@(MiniFrame _ th _) = th == headerOf tmf

prop_rowsOf :: MiniFrame -> Bool
prop_rowsOf tmf@(MiniFrame _ _ trs) = trs == rowsOf tmf

prop_rowsNum :: MiniFrame -> Bool
prop_rowsNum tmf = length (rowsOf tmf) == rowsNum tmf

prop_columnsNum :: MiniFrame -> Bool
prop_columnsNum tmf = length (columnsOf tmf) == columnsNum tmf

-- If rowsNum and columnsNum are fine, this should be fine too
prop_entriesNum :: MiniFrame -> Bool
prop_entriesNum tmf = rowsNum tmf * columnsNum tmf == entriesNum tmf

-- Helper function
cond :: Row -> MiniFrame -> Bool
cond tr tmf = null tr || null (rowsOf tmf) || length tr /= (length . head . rowsOf) tmf

prop_prependRow :: MiniFrame -> Row -> Bool
prop_prependRow tmf tr
    | cond tr tmf = True
    | otherwise   = rowsOf (prependRow tr tmf) == tr : rowsOf tmf

prop_prependColumn :: Name -> Column -> MiniFrame -> Bool
prop_prependColumn tn tc tmf
    | not (null $ rowsOf tmf) && length tc /= length (rowsOf tmf)     = True
    | otherwise                                                       = headerOf ntmf == tn : headerOf tmf && columnsOf ntmf == tc : columnsOf tmf
      where
        ntmf = prependColumn tn tc tmf

prop_appendRow :: MiniFrame -> Row -> Bool
prop_appendRow tmf tr
    | cond tr tmf = True
    | otherwise   = rowsOf (appendRow tr tmf) == rowsOf tmf ++ [tr]

-- Modification
-- , appendRow           -- Row -> MiniFrame -> MiniFrame
-- , appendColumn        -- Name -> Column -> MiniFrame -> MiniFrame
-- , insertRow           -- Index -> Row -> MiniFrame -> MiniFrame
-- , insertColumn        -- Name -> Name -> Column -> MiniFrame -> MiniFrame


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
                ]

    success <- all isSuccess <$> sequence tests
    unless success exitFailure
