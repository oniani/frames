{-
This is a test file for the MiniFrame.hs module.
Run 'cabal test' for running the tests.
-}

module Main where

import System.Exit               (exitFailure)
import Control.Monad             (unless)
import Control.Applicative       ((<$>), (<*>))
import Test.QuickCheck.Test      (quickCheckResult, isSuccess)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import MiniFrame.Frames

import qualified Data.List as List
import qualified Data.Maybe as Maybe

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
    | null h                                                 = True
    | length h /= length rs                                  = True
    | length h /= length (head rs)                           = True
    | h /= List.nub h                                        = True
    | null rs                                                = True
    | any null rs                                            = True
    | False `elem` map ((== (length . head) rs) . length) rs = True
    | h == List.nub h && mf == fromRows n h rs               = True
    | otherwise                                              = False
    where
        mf = MiniFrame n h rs

prop_fromColumns :: Name -> Header -> [Column] -> Bool
prop_fromColumns n h cs
    | null h                                                 = True
    | length h /= length (List.transpose cs)                 = True
    | length h /= length (head (List.transpose cs))          = True
    | h /= List.nub h                                        = True
    | null cs                                                = True
    | any null cs                                            = True
    | False `elem` map ((== (length . head) cs) . length) cs = True
    | h == List.nub h && mf == fromColumns n h cs            = True
    | otherwise                                              = False
      where
        mf = MiniFrame n h (List.transpose cs)

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

prop_entriesNum :: MiniFrame -> Bool
prop_entriesNum mf = rowsNum mf * columnsNum mf == entriesNum mf

prop_prependRow :: Row -> MiniFrame -> Bool
prop_prependRow r mf@(MiniFrame _ _ rs)
    | null rs                      = True
    | null . head $ rs             = True
    | length r /= length (head rs) = True
    | otherwise                    = rowsOf (prependRow r mf) == r : rs

prop_prependColumn :: Name -> Column -> MiniFrame -> Bool
prop_prependColumn cn c mf@(MiniFrame _ h rs)
    | length c /= length rs = True
    | otherwise             = cond
      where
        cond  = cond1 && cond2
        nmf   = prependColumn cn c mf
        cond1 = headerOf nmf == cn : h
        cond2 = rowsOf   nmf == List.transpose (c : List.transpose rs)

prop_appendRow :: Row -> MiniFrame -> Bool
prop_appendRow r mf@(MiniFrame _ _ rs)
    | null rs                      = True
    | null . head $ rs             = True
    | length r /= length (head rs) = True
    | otherwise                    = rowsOf (appendRow r mf) == rs ++ [r]

prop_appendColumn :: Name -> Column -> MiniFrame -> Bool
prop_appendColumn cn c mf@(MiniFrame _ h rs)
    | length c /= length rs = True
    | otherwise             = cond
      where
        cond  = cond1 && cond2
        nmf   = appendColumn cn c mf
        cond1 = headerOf nmf == h ++ [cn]
        cond2 = rowsOf   nmf == List.transpose (List.transpose rs ++ [c])

prop_removeRowByIndex :: Int -> MiniFrame -> Bool
prop_removeRowByIndex i mf@(MiniFrame n h rs)
    | i < 0 || i > length rs = True
    | otherwise              = MiniFrame n h (take i rs ++ drop (i + 1) rs) == removeRowByIndex i mf

prop_removeColumnByName :: String -> MiniFrame -> Bool
prop_removeColumnByName cn mf@(MiniFrame n h rs)
    | cn `elem` h = MiniFrame n nh nrs == removeColumnByName cn mf
    | otherwise   = True
    where
      nh  = List.delete cn h
      i   = Maybe.fromJust $ List.elemIndex cn h
      nrs = List.transpose $ take i (List.transpose rs) ++ drop (i + 1) (List.transpose rs)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let tests = [ quickCheckResult prop_fromSample
                , quickCheckResult prop_fromNull
                , quickCheckResult prop_fromRows
                , quickCheckResult prop_fromColumns
                , quickCheckResult prop_nameOf
                , quickCheckResult prop_headerOf
                , quickCheckResult prop_rowsOf
                , quickCheckResult prop_rowsNum
                , quickCheckResult prop_columnsNum
                , quickCheckResult prop_entriesNum
                , quickCheckResult prop_prependRow
                , quickCheckResult prop_prependColumn
                , quickCheckResult prop_appendRow
                , quickCheckResult prop_appendColumn
                , quickCheckResult prop_removeRowByIndex
                , quickCheckResult prop_removeColumnByName
                ]

    success <- all isSuccess <$> sequence tests
    unless success exitFailure
