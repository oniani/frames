{-
This is a test file for the MiniFrame.hs module.
Run 'cabal test' for running the tests.
-}

module Main where

import System.Exit          (exitFailure)
import Control.Applicative  ((<$>))
import Control.Monad        (unless)
import Data.List            (transpose, elemIndex)
import Data.Maybe           (fromJust)
import Test.QuickCheck.Test (quickCheckResult, isSuccess)
import MiniFrame

-- ----------------------------------------------------------------------------------------------------

-- STUPID TESTS FOR CHECKING. THESE HAVE TO BE REWRITTEN.

-- ----------------------------------------------------------------------------------------------------


prop_fromRows :: Name -> Header -> [Row] -> Bool
prop_fromRows testName testHeader testRows = miniframe == fromRows testName testHeader testRows
    where
        miniframe = MiniFrame testName testHeader testRows

prop_fromColumns :: Name -> Header -> [Column] -> Bool
prop_fromColumns testName testHeader testRows = miniframe == fromColumns testName testHeader testRows
    where
        miniframe = MiniFrame testName testHeader (transpose testRows)

-- prop_fromCSV :: IO Bool
-- prop_fromCSV = do
    -- io <- fromCSV "test.csv"
    -- MiniFrame "MiniFrame" ["Mumble1","Mumble2","Mumble3"] [["1","2","3"],["4","5","6"],["7","8","9"]]

-- Test name retrieval
prop_name :: Name -> Header -> [Row] -> Bool
prop_name testName testHeader testRows = miniframe == MiniFrame (name miniframe) testHeader testRows
    where
        miniframe = MiniFrame testName testHeader testRows

-- Test header retrieval
prop_header :: Name -> Header -> [Row] -> Bool
prop_header testName testHeader testRows = miniframe == MiniFrame testName (header miniframe) testRows
    where
        miniframe = MiniFrame testName testHeader testRows

-- Test rows retrieval
prop_rows :: Name -> Header -> [Row] -> Bool
prop_rows testName testHeader testRows = miniframe == MiniFrame testName testHeader (rows miniframe)
    where
        miniframe = MiniFrame testName testHeader testRows

-- Test columns retrieval
prop_columns :: Name -> Header -> [Row] -> Bool
prop_columns testName testHeader testRows = transpose testRows == columns miniframe
    where
        miniframe = MiniFrame testName testHeader testRows

-- Test row by ID retrieval
prop_rowByID :: Name -> Header -> [Row] -> ID -> Bool
prop_rowByID testName testHeader testRows testID
    | testID >= 0 && testID <= length testRows - 1 = length (head testRows) == length (rowByID miniframe testID)
    | otherwise                                    = 0 == length (rowByID miniframe testID)
    where
        miniframe = MiniFrame testName testHeader testRows

-- Test column by name retrieval
prop_columnByName :: Name -> Header -> [Row] -> Name -> Bool
prop_columnByName testName testHeader testRows testColumnName
    | testColumnName `elem` testHeader = transpose testRows !! index == columnByName miniframe testColumnName
    | otherwise                        = [] == columnByName miniframe testColumnName
    where
        miniframe = MiniFrame testName testHeader testRows
        index = fromJust (elemIndex testColumnName testHeader)

-- Test rows number retrieval
prop_rowsNum :: Name -> Header -> [Row] -> Bool
prop_rowsNum testName testHeader testRows = length testRows == rowsNum miniframe
    where
        miniframe = MiniFrame testName testHeader testRows

-- Test columns number retrieval
prop_columnsNum :: Name -> Header -> [Row] -> Bool
prop_columnsNum testName testHeader testRows = length (transpose testRows) == columnsNum miniframe
    where
        miniframe = MiniFrame testName testHeader testRows

-- Test entries number retrieval
prop_entriesNum :: Name -> Header -> [Row] -> Bool
prop_entriesNum testName testHeader testRows = rowsNum miniframe * columnsNum miniframe == entriesNum miniframe
    where
        miniframe = MiniFrame testName testHeader testRows

-- ----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "\nTesting starts!\n"
    let tests = [ quickCheckResult prop_fromRows
                , quickCheckResult prop_fromColumns
                , quickCheckResult prop_name
                , quickCheckResult prop_header
                , quickCheckResult prop_rows
                , quickCheckResult prop_columns
                , quickCheckResult prop_columnByName
                , quickCheckResult prop_rowsNum
                , quickCheckResult prop_columnsNum
                , quickCheckResult prop_entriesNum
                ]

    success <- all isSuccess <$> sequence tests
    unless success exitFailure
