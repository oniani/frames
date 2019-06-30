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
import Parse
import PrettyPrint

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

type Index  = Int
type Name   = String
type Header = [String]
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
fromSample = Miniframe name header rows
  where
    name   = "Miniframe"
    header = ["C1","C2","C3","C4"]
    rows   = [["R1-C1","R1-C2","R1-C3","R1-C4"],
              ["R2-C1","R2-C2","R2-C3","R2-C4"],
              ["R3-C1","R3-C2","R3-C3","R3-C4"],
              ["R4-C1","R4-C2","R4-C3","R4-C4"],
              ["R5-C1","R5-C2","R5-C3","R5-C4"],
              ["R6-C1","R6-C2","R6-C3","R6-C4"],
              ["R7-C1","R7-C2","R7-C3","R7-C4"],
              ["R8-C1","R8-C2","R8-C3","R8-C4"]]

-- | Built an empty miniframe
fromNull :: Miniframe
fromNull = Miniframe "" [] []

-- | Build from rows
fromRows :: Name -> Header -> [Row] -> Miniframe
fromRows name header rows
    | List.nub header /= header = error "Duplicate header names"
    | otherwise                 = Miniframe name header rows

-- | Build from columns
fromColumns :: Name -> Header -> [Column] -> Miniframe
fromColumns name header columns
    | List.nub header /= header = error "Duplicate header names"
    | otherwise                 = Miniframe name header $ List.transpose columns

-- | Build from the CSV file
fromCSV :: String -> IO Miniframe
fromCSV filename = do csvData <- readCSV filename
                      let header = head csvData
                      let rows   = tail csvData
                      return (Miniframe "Miniframe" header rows)

-------------------------------------------------------------------------------
-- Retrieval
-------------------------------------------------------------------------------

-- | Get the name
nameOf :: Miniframe -> Name
nameOf (Miniframe name _ _ ) = name

-- | Get the header
headerOf :: Miniframe -> Header
headerOf (Miniframe _ header _ ) = header

-- | Get the rows
rowsOf :: Miniframe -> [Row]
rowsOf (Miniframe _ _ rows ) = rows

-- | Get the columns
columnsOf :: Miniframe -> [Column]
columnsOf (Miniframe _ _ rows) = List.transpose rows

-- | Get the first entry
headOf :: Miniframe -> Row
headOf (Miniframe _ _ rows) = head rows

-- | Get the last entry
tailOf :: Miniframe -> [Row]
tailOf (Miniframe _ _ rows) = tail rows

-------------------------------------------------------------------------------
-- Dimensions
-------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: Miniframe -> Int
rowsNum (Miniframe _ _ rows) = length rows

-- | Get the number of columns
columnsNum :: Miniframe -> Int
columnsNum (Miniframe _ _ rows) = length $ List.transpose rows

-- | Get the number of entries
entriesNum :: Miniframe -> Int
entriesNum mf = rowsNum mf * columnsNum mf

-------------------------------------------------------------------------------
-- Addition
-------------------------------------------------------------------------------

-- | Add a row to the beginning
prependRow :: Row -> Miniframe -> Miniframe
prependRow newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise                                              = Miniframe name header (newRow : rows)

-- | Add a column to the beginning
prependColumn :: Name -> Column -> Miniframe -> Miniframe
prependColumn newColumnName newColumn (Miniframe name header rows)
    | not (null rows) && length newColumn /= length rows = error "Incompatible column size"
    | otherwise                                          = Miniframe name newHeader newRows
      where
        newHeader = newColumnName : header
        newRows   = List.transpose (newColumn : List.transpose rows)

-- | Add a row to the end
appendRow :: Row -> Miniframe -> Miniframe
appendRow newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise                                              = Miniframe name header (rows ++ [newRow])

-- | Add a column to the end
appendColumn :: Name -> Column -> Miniframe -> Miniframe
appendColumn newColumnName newColumn (Miniframe name header rows)
    | not (null rows) && length newColumn /= length rows = error "Incompatible column size"
    | otherwise                                          = Miniframe name newHeader newRows
      where
        newHeader = header ++ [newColumnName]
        newRows   = List.transpose (List.transpose rows ++ [newColumn])

-- | Insert a row at the given index
insertRow :: Index -> Row -> Miniframe -> Miniframe
insertRow index newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise                                              = Miniframe name header newRows
      where
        splitAtIndex = splitAt index rows
        newRows      = fst splitAtIndex ++ [newRow] ++ snd splitAtIndex

-- | Insert a column at the given index (index starts from 0)
insertColumn :: Index -> Name -> Column -> Miniframe -> Miniframe
insertColumn index newColumnName newColumn (Miniframe name header rows)
    | length newColumn /= length rows = error "Incompatible column size"
    | otherwise                       = Miniframe name newHeader newRows
      where
        splitI    = splitAt index $ List.transpose rows
        newRows   = List.transpose $ fst splitI ++ [newColumn] ++ snd splitI
        splitH    = splitAt index header
        newHeader = fst splitH ++ [newColumnName] ++ snd splitH

-------------------------------------------------------------------------------
-- Removal
-------------------------------------------------------------------------------

-- | Remove a row by index
removeRowByIndex :: Index -> Miniframe -> Miniframe
removeRowByIndex index mf@(Miniframe name header rows)
    | index < 0 || index >= length rows = mf
    | otherwise                         = Miniframe name header (take index rows ++ drop (index + 1) rows)

-- | Remove a column by name
removeColumnByName :: Name -> Miniframe -> Miniframe
removeColumnByName columnName mf@(Miniframe name header rows)
    | columnName `elem` header = Miniframe name newHeader newRows
    | otherwise                = mf
    where
      newHeader = List.delete columnName header
      index     = Maybe.fromJust $ List.elemIndex columnName header
      newRows   = List.transpose $ take index (List.transpose rows) ++ drop (index + 1) (List.transpose rows)

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert a column of strings to a column of integers
toInt :: Column -> [Int]
toInt xs
    | all (all isDigit) xs    = map (\x -> read x::Int) xs
    | otherwise               = error "Non-integer value in the column"

-- | Convert a column of strings to a column of decimals
toDecimal :: Column -> [Float]
toDecimal xs
    | all (all isDigit) $ map (filter (/='.')) xs = map (\x -> read x::Float) xs
    | otherwise               = error "Non-decimal value in the column"

-- | Convert a column of strings to a column of big integers
toBigInt :: Column -> [Integer]
toBigInt xs
    | all (all isDigit) xs    = map (\x -> read x::Integer) xs
    | otherwise               = error "Non-integer value in the column"

-- | Convert a column of strings to a column of big decimals
toBigDecimal :: Column -> [Double]
toBigDecimal xs
    | all (all isDigit) $ map (filter (/='.')) xs = map (\x -> read x::Double) xs
    | otherwise               = error "Non-decimal value in the column"

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
printRow index mf = prettyPrint2D [rowByIndex index mf]

-- | Print the rows
printRows :: Miniframe -> IO ()
printRows mf = prettyPrint2D $ rowsOf mf

-- | Print the column by name
printColumn :: Name -> Miniframe -> IO ()
printColumn name mf = prettyPrint1DV $ columnByName name mf

-- | Print the columns
printColumns :: Miniframe -> IO ()
printColumns mf = prettyPrint2D $ rowsOf mf

-- | Print the miniframe
printMf :: Miniframe -> IO ()
printMf (Miniframe name header rows) = do
    coloredPutStrLn $ name ++ "\n"
    prettyPrint2D   $ newHeader : newRows
    where
      newHeader = "" : header
      newRows   = [uncurry (:) p | p <- zip (map show [0..length rows - 1]) rows]

-------------------------------------------------------------------------------
-- Additional operations
-------------------------------------------------------------------------------

-- | Get a row by index
rowByIndex :: Index -> Miniframe -> Row
rowByIndex index (Miniframe _ _ rows)
    | index < 0 || index >= length rows = error "Index out of bounds"
    | otherwise                         = rows !! index

-- | Get a column by name
columnByName :: Name -> Miniframe -> Column
columnByName columnName (Miniframe _ header rows)
    | columnName `notElem` header = error "Unknown column name"
    | otherwise                   = List.transpose rows !! index
      where
        index = Maybe.fromJust $ List.elemIndex columnName header

-- | Get the column by index
columnByIndex :: Index -> Miniframe -> Column
columnByIndex index (Miniframe _ _ rows)
    | index < 0 || index >= length (List.transpose rows) = error "Index out of bounds"
    | otherwise                                          = List.transpose rows !! index

-- | Rename the miniframe
renameMf :: Name -> Miniframe -> Miniframe
renameMf newName (Miniframe _ header rows) = Miniframe newName header rows
