{-
This is a test file for the Miniframe.hs module.
Run 'cabal test' for running the tests.
-}

module Main where

import System.Exit          (exitFailure)
import Control.Applicative  ((<$>))
import Control.Monad        (unless)
import Data.List            (transpose, elemIndex)
import Data.Maybe           (fromJust)
import Test.QuickCheck.Test (quickCheckResult, isSuccess)
import Miniframe

-------------------------------------------------------------------------------
-- STUPID TESTS FOR CHECKING. THESE HAVE TO BE REWRITTEN.
-------------------------------------------------------------------------------

prop_fromNull :: Bool
prop_fromNull = fromNull == Miniframe "" [] []

prop_fromRows :: Name -> Header -> [Row] -> Bool
prop_fromRows tname theader trows = mf == fromRows tname theader trows
    where
        mf = Miniframe tname theader trows

prop_fromColumns :: Name -> Header -> [Column] -> Bool
prop_fromColumns tname theader tcolumns = mf == fromColumns tname theader tcolumns
    where
        mf = Miniframe tname theader (transpose tcolumns)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "\nTesting starts!\n"
    let tests = [ quickCheckResult prop_fromNull
                , quickCheckResult prop_fromRows
                ]

    success <- all isSuccess <$> sequence tests
    unless success exitFailure
