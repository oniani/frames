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
    , sample                    -- -> Miniframe
    , fromNull                  -- -> Miniframe
    , fromRows                  -- Name -> Header -> [Row] -> Miniframe
    , fromColumns               -- Name -> Header -> [Column] -> Miniframe
    , fromCSV                   -- String -> IO Miniframe

    -- Retrieval
    , nameOf                    -- Miniframe -> Name
    , headerOf                  -- Miniframe -> Header
    , rowsOf                    -- Miniframe -> [Row]
    , columnsOf                 -- Miniframe -> [Column]
    , rowByID                   -- ID -> Miniframe -> Row
    , columnByName              -- Name -> Miniframe -> Column

    -- Dimensions
    , rowsNum                   -- Miniframe -> Int
    , columnsNum                -- Miniframe -> Int
    , entriesNum                -- Miniframe -> Int

    -- Modification
    , renameMf                  -- Name -> Miniframe -> Miniframe
    , prependRow                -- Row -> Miniframe -> Miniframe
    , appendRow                 -- Row -> Miniframe -> Miniframe
    , insertRow                 -- ID -> Row -> Miniframe -> Miniframe
    , prependColumn             -- Name -> Column -> Miniframe -> Miniframe
    , appendColumn              -- Name -> Column -> Miniframe -> Miniframe
    , insertColumn              -- Name -> Name -> Column -> Miniframe -> Miniframe

    -- Removal
    , removeRowByID             -- ID -> Miniframe -> Miniframe
    , removeColumnByName        -- Name -> Miniframe -> Miniframe

    -- Pretty-printing
    , printName                 -- Miniframe -> IO ()
    , printHeader               -- Miniframe -> IO ()
    , printRow                  -- ID -> Miniframe -> IO ()
    , printRows                 -- Miniframe -> IO ()
    -- , printColumn               -- Name -> Miniframe -> IO ()
    -- , printColumns              -- Miniframe -> IO ()
    , printMf                   -- Miniframe -> IO ()
    ) where

import Data.Char (toLower)
import ParseCSV
import PrettyPrint

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

type ID     = Int
type Name   = String
type Header = [String]
type Row    = [String]
type Column = [String]

data Miniframe = Miniframe
    { _name   :: {-# UNPACK #-} !Name     -- Name of the Miniframe
    , _header :: {-# UNPACK #-} !Header   -- Header columns of the Miniframe
    , _rows   :: {-# UNPACK #-} ![Row] }  -- Rows of the Miniframe
    deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | A sample miniframe
sample :: Miniframe
sample = Miniframe name header rows
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

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

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
fromCSV filename
    | format /= ".csv" = error "Unknown file format"
    | otherwise        = do csvData <- readCSV filename
                            let header = head csvData
                            let rows   = tail csvData
                            return (Miniframe "Miniframe" header rows)
      where
        format = map toLower $ drop (length filename - 4) filename

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
tailOf :: Miniframe -> Row
tailOf (Miniframe _ _ rows) = last rows

-- | Get a row by ID
rowByID :: ID -> Miniframe -> Row
rowByID i (Miniframe _ _ rows)
    | i < 0 || i >= length rows = error "Index out of bounds"
    | otherwise                 = rows !! i

-- | Get a column by name
columnByName :: Name -> Miniframe -> Column
columnByName columnName (Miniframe _ header rows)
    | columnName `notElem` header = error "Unknown column name"
    | otherwise                   = List.transpose rows !! index
      where
        index = Maybe.fromJust $ List.elemIndex columnName header

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

-- | Rename the miniframe
renameMf :: Name -> Miniframe -> Miniframe
renameMf newName (Miniframe _ header rows) = Miniframe newName header rows

-- | Add a row to the beginning
prependRow :: Row -> Miniframe -> Miniframe
prependRow newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise                                              = Miniframe name header (newRow : rows)

-- | Add a row to the end
appendRow :: Row -> Miniframe -> Miniframe
appendRow newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise                                              = Miniframe name header (rows ++ [newRow])


-- | Insert a row at the given ID
insertRow :: ID -> Row -> Miniframe -> Miniframe
insertRow i newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise = Miniframe name header newRows
      where
        splitID = splitAt i rows
        newRows = fst splitID ++ [newRow] ++ snd splitID

-- | Add a column to the beginning
prependColumn :: Name -> Column -> Miniframe -> Miniframe
prependColumn newColumnName newColumn (Miniframe name header rows)
    | not (null rows) && length newColumn /= length rows = error "Incompatible column size"
    | otherwise                                          = Miniframe name newHeader newRows
      where
        newHeader = newColumnName : header
        newRows   = List.transpose (newColumn : List.transpose rows)

-- | Add a column to the end
appendColumn :: Name -> Column -> Miniframe -> Miniframe
appendColumn newColumnName newColumn (Miniframe name header rows)
    | not (null rows) && length newColumn /= length rows = error "Incompatible column size"
    | otherwise                                          = Miniframe name newHeader newRows
      where
        newHeader = header ++ [newColumnName]
        newRows   = List.transpose (List.transpose rows ++ [newColumn])

-- | Insert a column at the given index (index starts from 0)
insertColumn :: ID -> Name -> Column -> Miniframe -> Miniframe
insertColumn index newColumnName newColumn (Miniframe name header rows)
    | length newColumn /= length rows = error "Incompatible column size"
    | otherwise = Miniframe name newHeader newRows
      where
        splitI    = splitAt index $ List.transpose rows
        newRows   = List.transpose $ fst splitI ++ [newColumn] ++ snd splitI
        splitH    = splitAt index header
        newHeader = fst splitH ++ [newColumnName] ++ snd splitH

-------------------------------------------------------------------------------
-- Removal
-------------------------------------------------------------------------------

-- | Remove a row by ID
removeRowByID :: ID -> Miniframe -> Miniframe
removeRowByID i mf@(Miniframe name header rows)
    | i < 0 || i >= length rows = mf
    | otherwise                 = Miniframe name header (take i rows ++ drop (i + 1) rows)

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
-- Pretty-printing
-------------------------------------------------------------------------------

-- | Print the name
printName :: Miniframe -> IO ()
printName mf = coloredPutStrLn $ nameOf mf

-- | Print the header
printHeader :: Miniframe -> IO ()
printHeader mf = prettyPrint1D $ headerOf mf

-- | Print the row by id
printRow :: ID -> Miniframe -> IO ()
printRow  i mf = prettyPrint2D [rowByID i mf]

-- | Print the rows
printRows :: Miniframe -> IO ()
printRows mf = prettyPrint2D $ rowsOf mf

-- | Print the miniframe
printMf :: Miniframe -> IO ()
printMf (Miniframe name header rows) = do
    coloredPutStrLn $ name ++ "\n"
    prettyPrint2D   $ newHeader : newRows
    where
      newHeader = "ID" : header
      newRows   = [uncurry (:) p | p <- zip (map show [0..length rows - 1]) rows]
