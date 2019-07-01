{- |
Module      :  Miniframe.hs
Description :  Minimal data frames
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

An implementation of a data frame which comes with various handy operations.
-}

module Miniframe
    (
    -- Data types
      Miniframe (..)
    , Name
    , Header
    , Row
    , Column

    -- Construction
    , fromSample          -- Miniframe
    , fromNull            -- Miniframe
    , fromRows            -- Name -> Header -> [Row] -> Miniframe
    , fromColumns         -- Name -> Header -> [Column] -> Miniframe
    , fromCSV             -- String -> IO Miniframe

    -- Retrieval
    , nameOf              -- Miniframe -> Name
    , headerOf            -- Miniframe -> Header
    , rowsOf              -- Miniframe -> [Row]
    , columnsOf           -- Miniframe -> [Column]
    , headOf              -- Miniframe -> Row
    , tailOf              -- Miniframe -> [Row]

    -- Dimensions
    , rowsNum             -- Miniframe -> Int
    , columnsNum          -- Miniframe -> Int
    , entriesNum          -- Miniframe -> Int

    -- Modification
    , prependRow          -- Row -> Miniframe -> Miniframe
    , prependColumn       -- Name -> Column -> Miniframe -> Miniframe
    , appendRow           -- Row -> Miniframe -> Miniframe
    , appendColumn        -- Name -> Column -> Miniframe -> Miniframe
    , insertRow           -- Index -> Row -> Miniframe -> Miniframe
    , insertColumn        -- Name -> Name -> Column -> Miniframe -> Miniframe

    -- Removal
    , removeRowByIndex    -- Index -> Miniframe -> Miniframe
    , removeColumnByName  -- Name -> Miniframe -> Miniframe

    -- Conversion
    , toInt               -- Column -> [Int]
    , toDecimal           -- Column -> [Float]
    , toBigInt            -- Column -> [Integer]
    , toBigDecimal        -- Column -> [Double]

    -- Pretty-printing
    , printName           -- Miniframe -> IO ()
    , printHeader         -- Miniframe -> IO ()
    , printRow            -- Index -> Miniframe -> IO ()
    , printRows           -- Miniframe -> IO ()
    , printColumn         -- Name -> Miniframe -> IO ()
    , printColumns        -- Miniframe -> IO ()
    , printMf             -- Miniframe -> IO ()

    -- Additional operations
    , rowByIndex          -- Index -> Miniframe -> Row
    , columnByName        -- Name -> Miniframe -> Column
    , columnByIndex       -- Index -> Miniframe -> Column
    , renameMf            -- Name -> Miniframe -> Miniframe
    ) where

import Data.Char (isDigit)
import Optimize  (ordNub)
import Parse
import PrettyPrint

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

type Index  = Int
type Name   = String
type Header = [Name]
type Row    = [String]
type Column = [String]

data Miniframe = Miniframe
    { _name   :: !Name     -- Name of the Miniframe
    , _header :: !Header   -- Header columns of the Miniframe
    , _rows   :: ![Row] }  -- Rows of the Miniframe
    deriving (Eq,Show)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | A sample miniframe
fromSample :: Miniframe
fromSample = Miniframe n h rs
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
         , ["R8-C1","R8-C2","R8-C3","R8-C4"] ]

-- | Built an empty miniframe
fromNull :: Miniframe
fromNull = Miniframe "" [] []

-- | Build from rows
fromRows :: Name -> Header -> [Row] -> Miniframe
fromRows n h rs
    | ordNub h /= h = error "Duplicate header names"
    | otherwise     = Miniframe n h rs

-- | Build from columns
fromColumns :: Name -> Header -> [Column] -> Miniframe
fromColumns n h cs
    | ordNub h /= h = error "Duplicate header names"
    | otherwise     = Miniframe n h $ List.transpose cs

-- | Build from the CSV file
fromCSV :: String -> IO Miniframe
fromCSV fn = do
    csv <- readCSV fn
    let h  = head csv
    let rs = tail csv
    return (Miniframe "Miniframe" h rs)

-------------------------------------------------------------------------------
-- Retrieval
-------------------------------------------------------------------------------

-- | Get the name
nameOf :: Miniframe -> Name
nameOf (Miniframe n _ _ ) = n

-- | Get the header
headerOf :: Miniframe -> Header
headerOf (Miniframe _ h _ ) = h

-- | Get the rows
rowsOf :: Miniframe -> [Row]
rowsOf (Miniframe _ _ rs ) = rs

-- | Get the columns
columnsOf :: Miniframe -> [Column]
columnsOf (Miniframe _ _ rs) = List.transpose rs

-- | Get the first entry
headOf :: Miniframe -> Row
headOf (Miniframe _ _ rs) = head rs

-- | Get the last entry
tailOf :: Miniframe -> [Row]
tailOf (Miniframe _ _ rs) = tail rs

-------------------------------------------------------------------------------
-- Dimensions
-------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: Miniframe -> Int
rowsNum (Miniframe _ _ rs) = length rs

-- | Get the number of columns
columnsNum :: Miniframe -> Int
columnsNum (Miniframe _ _ rs) = length $ List.transpose rs

-- | Get the number of entries
entriesNum :: Miniframe -> Int
entriesNum mf = rowsNum mf * columnsNum mf

-------------------------------------------------------------------------------
-- Addition
-------------------------------------------------------------------------------

-- | Add a row to the beginning
prependRow :: Row -> Miniframe -> Miniframe
prependRow nr (Miniframe n h rs)
    | not (null rs) && length nr /= length (head rs) = error "Incompatible row size"
    | otherwise                                      = Miniframe n h (nr : rs)

-- | Add a column to the beginning
prependColumn :: Name -> Column -> Miniframe -> Miniframe
prependColumn ncn nc (Miniframe n h rs)
    | not (null rs) && length nc /= length rs = error "Incompatible column size"
    | otherwise                                 = Miniframe n nh nrs
      where
        nh  = ncn : h
        nrs = List.transpose (nc : List.transpose rs)

-- | Add a row to the end
appendRow :: Row -> Miniframe -> Miniframe
appendRow nr (Miniframe n h rs)
    | not (null rs) && length nr /= length (head rs) = error "Incompatible row size"
    | otherwise                                      = Miniframe n h (rs ++ [nr])

-- | Add a column to the end
appendColumn :: Name -> Column -> Miniframe -> Miniframe
appendColumn ncn nc (Miniframe n h rs)
    | not (null rs) && length nc /= length rs = error "Incompatible column size"
    | otherwise                               = Miniframe n nh nrs
      where
        nh  = h ++ [ncn]
        nrs = List.transpose (List.transpose rs ++ [nc])

-- | Insert a row at the given index
insertRow :: Index -> Row -> Miniframe -> Miniframe
insertRow i nr (Miniframe n h rs)
    | not (null rs) && length nr /= length (head rs) = error "Incompatible row size"
    | otherwise                                      = Miniframe n h nrs
      where
        srs = splitAt i rs
        nrs = fst srs ++ [nr] ++ snd srs

-- | Insert a column at the given index (index starts from 0)
insertColumn :: Index -> Name -> Column -> Miniframe -> Miniframe
insertColumn i ncn nc (Miniframe n h rs)
    | length nc /= length rs = error "Incompatible column size"
    | otherwise              = Miniframe n nh nrs
      where
        srs = splitAt i $ List.transpose rs
        nrs = List.transpose $ fst srs ++ [nc] ++ snd srs
        sh  = splitAt i h
        nh  = fst sh ++ [ncn] ++ snd sh

-------------------------------------------------------------------------------
-- Removal
-------------------------------------------------------------------------------

-- | Remove a row by index
removeRowByIndex :: Index -> Miniframe -> Miniframe
removeRowByIndex i mf@(Miniframe n h rs)
    | i < 0 || i >= length rs = mf
    | otherwise               = Miniframe n h (take i rs ++ drop (i + 1) rs)

-- | Remove a column by name
removeColumnByName :: Name -> Miniframe -> Miniframe
removeColumnByName cn mf@(Miniframe n h rs)
    | cn `elem` h = Miniframe n nh nrs
    | otherwise   = mf
    where
      nh  = List.delete cn h
      i   = Maybe.fromJust $ List.elemIndex cn h
      nrs = List.transpose $ take i (List.transpose rs) ++ drop (i + 1) (List.transpose rs)

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert a column of strings to a column of integers
toInt :: Column -> [Int]
toInt c
    | all (all isDigit) c = map (\n -> read n :: Int) c
    | otherwise           = error "Non-integer value in the column"

-- | Convert a column of strings to a column of decimals
toDecimal :: Column -> [Float]
toDecimal c
    | all (all isDigit) $ map (filter (/='.')) c = map (\n -> read n :: Float) c
    | otherwise                                  = error "Non-decimal value in the column"

-- | Convert a column of strings to a column of big integers
toBigInt :: Column -> [Integer]
toBigInt c
    | all (all isDigit) c = map (\n -> read n :: Integer) c
    | otherwise           = error "Non-integer value in the column"

-- | Convert a column of strings to a column of big decimals
toBigDecimal :: Column -> [Double]
toBigDecimal c
    | all (all isDigit) $ map (filter (/='.')) c = map (\n -> read n :: Double) c
    | otherwise                                  = error "Non-decimal value in the column"

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

-- | Print the name
printName :: Miniframe -> IO ()
printName mf = coloredPutStrLn $ nameOf mf

-- | Print the header
printHeader :: Miniframe -> IO ()
printHeader mf = prettyPrint1D $ headerOf mf

-- | Print the row by index
printRow :: Index -> Miniframe -> IO ()
printRow i mf = prettyPrint2D [rowByIndex i mf]

-- | Print the rows
printRows :: Miniframe -> IO ()
printRows mf = prettyPrint2D $ rowsOf mf

-- | Print the column by name
printColumn :: Name -> Miniframe -> IO ()
printColumn n mf = prettyPrint1DV $ columnByName n mf

-- | Print the columns
printColumns :: Miniframe -> IO ()
printColumns mf = prettyPrint2D $ rowsOf mf

-- | Print the miniframe
printMf :: Miniframe -> IO ()
printMf (Miniframe n h rs) = do
    coloredPutStrLn $ n ++ "\n"
    prettyPrint2D   $ nh : nrs
    where
      nh  = "" : h
      nrs = [uncurry (:) p | p <- zip (map show [0..length rs - 1]) rs]

-------------------------------------------------------------------------------
-- Additional operations
-------------------------------------------------------------------------------

-- | Get a row by index
rowByIndex :: Index -> Miniframe -> Row
rowByIndex i (Miniframe _ _ rs)
    | i < 0 || i >= length rs = error "Index out of bounds"
    | otherwise               = rs !! i

-- | Get a column by name
columnByName :: Name -> Miniframe -> Column
columnByName cn (Miniframe _ h rs)
    | cn `notElem` h = error "Unknown column name"
    | otherwise      = List.transpose rs !! i
      where
        i = Maybe.fromJust $ List.elemIndex cn h

-- | Get the column by index
columnByIndex :: Index -> Miniframe -> Column
columnByIndex i (Miniframe _ _ rs)
    | i < 0 || i >= length (List.transpose rs) = error "Index out of bounds"
    | otherwise                                = List.transpose rs !! i

-- | Rename the miniframe
renameMf :: Name -> Miniframe -> Miniframe
renameMf nn (Miniframe _ h rs) = Miniframe nn h rs
