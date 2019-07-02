{-
This is a test file for the Miniframe.hs module.
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

import Miniframe

import qualified Data.List as List

instance Arbitrary Miniframe where
    arbitrary = do  a <- arbitrary
                    b <- arbitrary
                    c <- arbitrary
                    return $ Miniframe a b c

-------------------------------------------------------------------------------

prop_fromSample :: Bool
prop_fromSample = fromSample == Miniframe n h rs
  where
    n  = "Miniframe"
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
prop_fromNull = fromNull == Miniframe "" [] []

prop_fromRows :: Name -> Header -> [Row] -> Bool
prop_fromRows tn th trs
    | th /= List.nub th                             = True  -- Bypass
    | th == List.nub th && mf == fromRows tn th trs = True
    | otherwise                                     = False
    where
        mf = Miniframe tn th trs

prop_fromColumns :: Name -> Header -> [Row] -> Bool
prop_fromColumns tn th trs
    | th /= List.nub th                                = True  -- Bypass
    | th == List.nub th && mf == fromColumns tn th trs = True
    | otherwise                                        = False
    where
        mf = Miniframe tn th (List.transpose trs)

prop_nameOf :: Miniframe -> Bool
prop_nameOf tmf@(Miniframe tn _ _) = tn == nameOf tmf

prop_headerOf :: Miniframe -> Bool
prop_headerOf tmf@(Miniframe _ th _) = th == headerOf tmf

prop_rowsOf :: Miniframe -> Bool
prop_rowsOf tmf@(Miniframe _ _ trs) = trs == rowsOf tmf

prop_rowsNum :: Miniframe -> Bool
prop_rowsNum tmf = length (rowsOf tmf) == rowsNum tmf

prop_columnsNum :: Miniframe -> Bool
prop_columnsNum tmf = length (columnsOf tmf) == columnsNum tmf

-- If rowsNum and columnsNum are fine, this should be fine too
prop_entriesNum :: Miniframe -> Bool
prop_entriesNum tmf = rowsNum tmf * columnsNum tmf == entriesNum tmf

-- Helper function
cond :: Row -> Miniframe -> Bool
cond tr tmf = null tr || null (rowsOf tmf) || length tr /= (length . head . rowsOf) tmf

prop_prependRow :: Miniframe -> Row -> Bool
prop_prependRow tmf tr
    | cond tr tmf = True
    | otherwise   = rowsOf (prependRow tr tmf) == tr : rowsOf tmf

{-
prop_prependColumn :: Miniframe -> Row -> Bool
prop_prependColumn mf nc
    | cond (List.transpose [c]) = True
    | otherwise = rowsOf (prependRow r mf) == r : rowsOf mf
-}

prop_appendRow :: Miniframe -> Row -> Bool
prop_appendRow tmf tr
    | cond tr tmf = True
    | otherwise   = rowsOf (appendRow tr tmf) == rowsOf tmf ++ [tr]

-- Modification
-- , appendRow           -- Row -> Miniframe -> Miniframe
-- , appendColumn        -- Name -> Column -> Miniframe -> Miniframe
-- , insertRow           -- Index -> Row -> Miniframe -> Miniframe
-- , insertColumn        -- Name -> Name -> Column -> Miniframe -> Miniframe


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
                , quickCheckResult prop_appendRow
                ]

    success <- all isSuccess <$> sequence tests
    unless success exitFailure
