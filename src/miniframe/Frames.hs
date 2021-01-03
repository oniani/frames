{- |
Module      :  Frames.hs
Description :  Minimal data frames
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

An implementation of a data frame which comes with various handy operations.
-}

module MiniFrame.Frames
    ( MiniFrame (..)
    , Name
    , Header
    , Row
    , Column

    -- * Construction
    , fromSample          -- MiniFrame
    , fromNull            -- MiniFrame
    , fromRows            -- Name -> Header -> [Row] -> MiniFrame
    , fromColumns         -- Name -> Header -> [Column] -> MiniFrame
    , fromCSV             -- String -> IO MiniFrame

    -- * Retrieval
    , nameOf              -- MiniFrame -> Name
    , headerOf            -- MiniFrame -> Header
    , rowsOf              -- MiniFrame -> [Row]
    , columnsOf           -- MiniFrame -> [Column]
    , headOf              -- MiniFrame -> Row
    , tailOf              -- MiniFrame -> [Row]

    -- * Dimensions
    , rowsNum             -- MiniFrame -> Int
    , columnsNum          -- MiniFrame -> Int
    , entriesNum          -- MiniFrame -> Int

    -- * Modification
    , prependRow          -- Row -> MiniFrame -> MiniFrame
    , prependColumn       -- Name -> Column -> MiniFrame -> MiniFrame
    , appendRow           -- Row -> MiniFrame -> MiniFrame
    , appendColumn        -- Name -> Column -> MiniFrame -> MiniFrame
    , insertRow           -- Index -> Row -> MiniFrame -> MiniFrame
    , insertColumn        -- Name -> Name -> Column -> MiniFrame -> MiniFrame
    , renameMf            -- Name -> MiniFrame -> MiniFrame

    -- * Removal
    , removeRowByIndex    -- Index -> MiniFrame -> MiniFrame
    , removeColumnByName  -- Name -> MiniFrame -> MiniFrame

    -- * Pretty-printing
    , printName           -- MiniFrame -> IO ()
    , printHeader         -- MiniFrame -> IO ()
    , printRow            -- Index -> MiniFrame -> IO ()
    , printRows           -- MiniFrame -> IO ()
    , printColumn         -- Name -> MiniFrame -> IO ()
    , printColumns        -- MiniFrame -> IO ()
    , printMF             -- MiniFrame -> IO ()

    -- * Additional operations
    , rowByIndex          -- Index -> MiniFrame -> Row
    , columnByName        -- Name -> MiniFrame -> Column
    , columnByIndex       -- Index -> MiniFrame -> Column

    -- * Conversion
    , toInt               -- Column -> MiniFrame -> [Int]
    , toDecimal           -- Column -> MiniFrame -> [Float]
    , toBigInt            -- Column -> MiniFrame -> [Integer]
    , toBigDecimal        -- Column -> MiniFrame -> [Double]
    ) where

import Data.Char (isDigit)
import Parse
import PrettyPrint
import Util

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

type Index  = Int
type Name   = String
type Header = [Name]
type Row    = [String]
type Column = [String]

data MiniFrame = MiniFrame
    { name   :: !Name
    , header :: !Header
    , rows   :: ![Row]
    } deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | A sample miniframe
fromSample :: MiniFrame
fromSample = MiniFrame n h rs
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

-- | Build an empty miniframe
fromNull :: MiniFrame
fromNull = MiniFrame "" [] []

-- | Build from rows
fromRows :: Name -> Header -> [Row] -> MiniFrame
fromRows n h rs = constraintsRow n h rs $ MiniFrame n h rs

-- | Build from columns
fromColumns :: Name -> Header -> [Column] -> MiniFrame
fromColumns n h cs = constraintsColumn n h cs $ MiniFrame n h $ List.transpose cs

-- | Build from the CSV file
fromCSV :: String -> IO MiniFrame
fromCSV fn = do
    csv <- readCSV fn
    let h  = head csv
    let rs = tail csv
    return $ constraintsRow "MiniFrame" h rs $ MiniFrame "MiniFrame" h rs

-------------------------------------------------------------------------------
-- Retrieval
-------------------------------------------------------------------------------

-- | Get the name
nameOf :: MiniFrame -> Name
nameOf (MiniFrame n _ _ ) = n

-- | Get the header
headerOf :: MiniFrame -> Header
headerOf (MiniFrame _ h _ ) = h

-- | Get the rows
rowsOf :: MiniFrame -> [Row]
rowsOf (MiniFrame _ _ rs ) = rs

-- | Get the columns
columnsOf :: MiniFrame -> [Column]
columnsOf (MiniFrame _ _ rs) = List.transpose rs

-- | Get the first entry
headOf :: MiniFrame -> Row
headOf (MiniFrame _ _ rs) = head rs

-- | Get the last entry
tailOf :: MiniFrame -> [Row]
tailOf (MiniFrame _ _ rs) = tail rs

-------------------------------------------------------------------------------
-- Dimensions
-------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: MiniFrame -> Int
rowsNum (MiniFrame _ _ rs) = length rs

-- | Get the number of columns
columnsNum :: MiniFrame -> Int
columnsNum (MiniFrame _ _ rs) = length $ List.transpose rs

-- | Get the number of entries
entriesNum :: MiniFrame -> Int
entriesNum mf = rowsNum mf * columnsNum mf

-------------------------------------------------------------------------------
-- Addition
-------------------------------------------------------------------------------

-- | Add a row to the beginning
prependRow :: Row -> MiniFrame -> MiniFrame
prependRow r (MiniFrame n h rs)
    | length r /= length (head rs) = error "Incompatible row size"
    | otherwise                    = MiniFrame n h (r : rs)

-- | Add a column to the beginning
prependColumn :: Name -> Column -> MiniFrame -> MiniFrame
prependColumn cn c (MiniFrame n h rs)
    | length c /= length rs = error "Incompatible column size"
    | otherwise             = MiniFrame n nh nrs
      where
        nh  = cn : h
        nrs = List.transpose (c : List.transpose rs)

-- | Add a row to the end
appendRow :: Row -> MiniFrame -> MiniFrame
appendRow r (MiniFrame n h rs)
    | length r /= length (head rs) = error "Incompatible row size"
    | otherwise                    = MiniFrame n h (rs ++ [r])

-- | Add a column to the end
appendColumn :: Name -> Column -> MiniFrame -> MiniFrame
appendColumn cn c (MiniFrame n h rs)
    | length c /= length rs = error "Incompatible column size"
    | otherwise             = MiniFrame n nh nrs
      where
        nh  = h ++ [cn]
        nrs = List.transpose $ List.transpose rs ++ [c]

-- | Insert a row at the given index
insertRow :: Index -> Row -> MiniFrame -> MiniFrame
insertRow i r (MiniFrame n h rs)
    | length r /= length (head rs) = error "Incompatible row size"
    | i < 0 || i >= length rs      = error "Index out of bounds"
    | otherwise                    = MiniFrame n h nrs
      where
        srs = splitAt i rs
        nrs = fst srs ++ [r] ++ snd srs

-- | Insert a column at the given index (index starts from 0)
insertColumn :: Index -> Name -> Column -> MiniFrame -> MiniFrame
insertColumn i cn c (MiniFrame n h rs)
    | length c /= length rs          = error "Incompatible column size"
    | i < 0 || i >= length (head rs) = error "Index out of bounds"
    | otherwise                      = MiniFrame n nh nrs
      where
        srs = splitAt i $ List.transpose rs
        nrs = List.transpose $ fst srs ++ [c] ++ snd srs
        sh  = splitAt i h
        nh  = fst sh ++ [cn] ++ snd sh

-- | Rename the miniframe
renameMf :: Name -> MiniFrame -> MiniFrame
renameMf n (MiniFrame _ h rs) = MiniFrame n h rs

-------------------------------------------------------------------------------
-- Removal
-------------------------------------------------------------------------------

-- | Remove a row by index
removeRowByIndex :: Index -> MiniFrame -> MiniFrame
removeRowByIndex i (MiniFrame n h rs)
    | i < 0 || i > length rs = error "Index out of bounds"
    | otherwise              = MiniFrame n h $ take i rs ++ drop (i + 1) rs

-- | Remove a column by name
removeColumnByName :: Name -> MiniFrame -> MiniFrame
removeColumnByName cn (MiniFrame n h rs)
    | cn `elem` h = MiniFrame n nh nrs
    | otherwise   = error "Unknown column name"
    where
      nh  = List.delete cn h
      i   = Maybe.fromJust $ List.elemIndex cn h
      nrs = List.transpose $ take i (List.transpose rs) ++ drop (i + 1) (List.transpose rs)

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

-- | Print the name
printName :: MiniFrame -> IO ()
printName mf = coloredPutStrLn $ nameOf mf

-- | Print the header
printHeader :: MiniFrame -> IO ()
printHeader mf = prettyPrint1D $ headerOf mf

-- | Print the row by index
printRow :: Index -> MiniFrame -> IO ()
printRow i mf = prettyPrint2D [rowByIndex i mf]

-- | Print the rows
printRows :: MiniFrame -> IO ()
printRows mf = prettyPrint2D $ rowsOf mf

-- | Print the column by name
printColumn :: Name -> MiniFrame -> IO ()
printColumn n mf = prettyPrint1DV $ columnByName n mf

-- | Print the columns
printColumns :: MiniFrame -> IO ()
printColumns mf = prettyPrint2D $ rowsOf mf

-- | Print the miniframe
printMF :: MiniFrame -> IO ()
printMF (MiniFrame n h rs) = do
    coloredPutStrLn $ n ++ "\n"
    prettyPrint2D   $ nh : nrs
    where
      nh  = "" : h
      nrs = [uncurry (:) p | p <- zip (map show [0..length rs - 1]) rs]

-------------------------------------------------------------------------------
-- Additional operations
-------------------------------------------------------------------------------

-- | Get a row by index
rowByIndex :: Index -> MiniFrame -> Row
rowByIndex i (MiniFrame _ _ rs)
    | i < 0 || i >= length rs = error "Index out of bounds"
    | otherwise               = rs !! i

-- | Get a column by name
columnByName :: Name -> MiniFrame -> Column
columnByName cn (MiniFrame _ h rs)
    | cn `notElem` h = error "Unknown column name"
    | otherwise      = List.transpose rs !! i
      where
        i = Maybe.fromJust $ List.elemIndex cn h

-- | Get the column by index
columnByIndex :: Index -> MiniFrame -> Column
columnByIndex i (MiniFrame _ _ rs)
    | i < 0 || i >= length (List.transpose rs) = error "Index out of bounds"
    | otherwise                                = List.transpose rs !! i

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert a column of strings to a column of integers
toInt :: Name -> MiniFrame -> [Int]
toInt cn mf
    | all (all isDigit) c = map (\n -> read n :: Int) c
    | otherwise           = error "Non-integer value in the column"
      where
        c = columnByName cn mf

-- | Convert a column of strings to a column of decimals
toDecimal :: Name -> MiniFrame -> [Float]
toDecimal cn mf
    | all (all isDigit) $ map (filter (/='.')) c = map (\n -> read n :: Float) c
    | otherwise                                  = error "Non-decimal value in the column"
      where
        c = columnByName cn mf

-- | Convert a column of strings to a column of big integers
toBigInt :: Name -> MiniFrame -> [Integer]
toBigInt cn mf
    | all (all isDigit) c = map (\n -> read n :: Integer) c
    | otherwise           = error "Non-integer value in the column"
      where
        c = columnByName cn mf

-- | Convert a column of strings to a column of big decimals
toBigDecimal :: Name -> MiniFrame -> [Double]
toBigDecimal cn mf
    | all (all isDigit) $ map (filter (/='.')) c = map (\n -> read n :: Double) c
    | otherwise                                  = error "Non-decimal value in the column"
      where
        c = columnByName cn mf
