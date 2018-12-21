{-
This is a test file for the MiniFrame.hs module.
Run 'cabal test' for running the tests.
-}

module Main where

import System.Exit          (exitFailure)
import Control.Applicative  ((<$>))
import Control.Monad        (unless)
import Test.QuickCheck.Test (quickCheckResult, isSuccess)
import MiniFrame

-- ----------------------------------------------------------------------------------------------------

-- prop_fromRows :: Name -> Header -> [Row] -> Bool
-- prop_fromRows name header rows = MiniFrame name header rows == fromRows name header rows

prop_getName :: Name -> Header -> [Row] -> Bool
prop_getName name header rows = MiniFrame name header rows == MiniFrame name header rows

-- ----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Testing starts!"
    let tests = [ --quickCheckResult prop_fromRows
                  quickCheckResult prop_getName
                ]

    success <- all isSuccess <$> sequence tests
    unless success exitFailure
