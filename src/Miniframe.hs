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
    , name                      -- Miniframe -> Name
    , header                    -- Miniframe -> Header
    , rows                      -- Miniframe -> [Row]
    , columns                   -- Miniframe -> [Column]
    , rowByID                   -- Miniframe -> ID -> Row
    , columnByName              -- Miniframe -> Name -> Column

    -- Dimensions
    , rowsNum                   -- Miniframe -> Int
    , columnsNum                -- Miniframe -> Int
    , entriesNum                -- Miniframe -> Int

    -- Modification
    , renameMf                  -- Name -> Miniframe -> Miniframe
    , appendRow                 -- Row -> Miniframe -> Miniframe
    , prependRow                -- Row -> Miniframe -> Miniframe
    , appendColumn              -- Name -> Column -> Miniframe -> Miniframe
    , prependColumn             -- Name -> Column -> Miniframe -> Miniframe
    , insertRow                 -- ID -> Row -> Miniframe -> Miniframe
    , insertColumn              -- Name -> Name -> Column -> Miniframe -> Miniframe
    , removeRowByID             -- Miniframe -> ID -> Miniframe
    , removeColumnByName        -- Miniframe -> Name -> Miniframe

    -- Pretty-printing
    , printName                 -- Miniframe -> IO ()
    , printHeader               -- Miniframe -> IO ()
    , printRows                 -- Miniframe -> IO ()
    , printMf                   -- Miniframe -> IO ()
    ) where

import ParseCSV
import PrettyPrint

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Char  as Char

type ID     = Int
type Name   = String
type Header = [String]
type Row    = [String]
type Column = [String]

data Miniframe = Miniframe
    { _name   :: Name           -- Name of the Miniframe
    , _header :: Header         -- Header columns of the Miniframe
    , _rows   :: [Row] }        -- Rows of the Miniframe
    deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | A sample Miniframe
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

-- | Built an empty Miniframe (probably useless)
fromNull :: Miniframe
fromNull = Miniframe "" [] []

-- | Build a Miniframe from rows
fromRows :: Name -> Header -> [Row] -> Miniframe
fromRows name header rows
    | List.nub header == header = error "Duplicate header names"
    | otherwise                 = Miniframe name header rows

-- | Build a Miniframe from columns
fromColumns :: Name -> Header -> [Column] -> Miniframe
fromColumns name header columns
    | List.nub header == header = error "Duplicate header names"
    | otherwise                 = Miniframe name header (List.transpose columns)

-- | Build a Miniframe from the CSV file
fromCSV :: String -> IO Miniframe
fromCSV filename
  | format /= ".csv" = error "Unknown file format!"
  | otherwise        = do csvData <- readCSV filename
                          let header = head csvData
                          let rows   = tail csvData
                          return (Miniframe "Miniframe" header rows)
    where
        format = map Char.toLower $ drop (length filename - 4) filename

-------------------------------------------------------------------------------
-- Retrieval
-------------------------------------------------------------------------------

-- | Get the name
name :: Miniframe -> Name
name (Miniframe name _ _ ) = name

-- | Get the header
header :: Miniframe -> Header
header (Miniframe _ header _ ) = header

-- | Get the rows
rows :: Miniframe -> [Row]
rows (Miniframe _ _ rows ) = rows

-- | Get the columns
columns :: Miniframe -> [Column]
columns (Miniframe _ _ rows) = List.transpose rows

-- | Get the first entry
hd :: Miniframe -> Row
hd (Miniframe _ _ rows) = head rows

-- | Get the last entry
tl :: Miniframe -> Row
tl (Miniframe _ _ rows) = last rows

-- | Get a row by ID
rowByID :: Miniframe -> ID -> Row
rowByID (Miniframe _ _ rows) id
    | id < 0 || id >= length rows = error "Index out of bounds"
    | otherwise                   = rows !! id

-- | Get a column by name
columnByName :: Miniframe -> Name -> Column
columnByName (Miniframe _ header rows) columnName
    | columnName `elem` header = List.transpose rows !! index
    | otherwise                = error "Unknown column name"
    where
        index = Maybe.fromJust (List.elemIndex columnName header)

-------------------------------------------------------------------------------
-- Dimensions
-------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: Miniframe -> Int
rowsNum (Miniframe _ _ rows) = length rows

-- | Get the number of columns
columnsNum :: Miniframe -> Int
columnsNum (Miniframe _ _ rows) = length (List.transpose rows)

-- | Get the number of entries
entriesNum :: Miniframe -> Int
entriesNum mf = rowsNum mf * columnsNum mf

-------------------------------------------------------------------------------
-- Addition
-------------------------------------------------------------------------------

-- | Rename the Miniframe
renameMf :: Name -> Miniframe -> Miniframe
renameMf newName (Miniframe name header rows) = Miniframe newName header rows

-- | Add a row to the end of the Miniframe
appendRow :: Row -> Miniframe -> Miniframe
appendRow newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise                                              = Miniframe name header (rows ++ [newRow])

-- | Add a row to the beginning of the Miniframe
prependRow :: Row -> Miniframe -> Miniframe
prependRow newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise                                              = Miniframe name header (newRow : rows)

-- | Add a column to the end of the Miniframe
appendColumn :: Name -> Column -> Miniframe -> Miniframe
appendColumn newColumnName newColumn (Miniframe name header rows)
    | not (null rows) && length newColumn /= length rows = error "Incompatible column size"
    | otherwise                                          = Miniframe name newHeader newRows
    where
        newHeader = header ++ [newColumnName]
        newRows   = List.transpose (List.transpose rows ++ [newColumn])

-- | Add a column to the beginning of the Miniframe
prependColumn :: Name -> Column -> Miniframe -> Miniframe
prependColumn newColumnName newColumn (Miniframe name header rows)
    | not (null rows) && length newColumn /= length rows = error "Incompatible column size"
    | otherwise                                          = Miniframe name newHeader newRows
    where
        newHeader = newColumnName : header
        newRows   = List.transpose (newColumn : List.transpose rows)

-- | Insert a row at the given ID
insertRow :: ID -> Row -> Miniframe -> Miniframe
insertRow id newRow (Miniframe name header rows)
    | not (null rows) && length newRow /= length (head rows) = error "Incompatible row size"
    | otherwise = Miniframe name header newRows
    where
        splitID = splitAt id rows
        newRows = fst splitID ++ [newRow] ++ snd splitID

-- | Insert a column at the given index (index starts from 0)
insertColumn :: ID -> Name -> Row -> Miniframe -> Miniframe
insertColumn index newColumnName newColumn (Miniframe name header rows)
    | length newColumn /= length rows = error "Incompatible column size"
    | otherwise = Miniframe name newHeader newRows
        where
            splitI    = splitAt index (List.transpose rows)
            newRows   = List.transpose (fst splitI ++ [newColumn] ++ snd splitI)
            splitH    = splitAt index header
            newHeader = fst splitH ++ [newColumnName] ++ snd splitH

-------------------------------------------------------------------------------
-- Removal
-------------------------------------------------------------------------------

-- | Remove a row by ID
removeRowByID :: Miniframe -> ID -> Miniframe
removeRowByID mf@(Miniframe name header rows) id
    | id < 0 || id >= length rows = mf
    | otherwise                   = Miniframe name header (take id rows ++ drop (id + 1) rows)

-- | Remove a column by name
removeColumnByName :: Miniframe -> Name -> Miniframe
removeColumnByName mf@(Miniframe name header rows) columnName
    | columnName `elem` header = Miniframe name newHeader newRows
    | otherwise                = mf
    where
        newHeader = List.delete columnName header
        index     = Maybe.fromJust (List.elemIndex columnName header)
        newRows   = List.transpose (take index (List.transpose rows) ++ drop (index + 1) (List.transpose rows))

-------------------------------------------------------------------------------
-- Printing and Pretty-printing
-------------------------------------------------------------------------------

-- | Print the name of the Miniframe
printName :: Miniframe -> IO ()
printName (Miniframe name _ _) = coloredPutStrLn name

-- | Print the header of the Miniframe
printHeader :: Miniframe -> IO ()
printHeader (Miniframe _ header _) = prettyPrint1D header

-- | Print the rows of the Miniframe
printRows :: Miniframe -> IO ()
printRows (Miniframe _ _ rows) = prettyPrint2D rows

-- | Print the miniframe
printMf :: Miniframe -> IO ()
printMf (Miniframe name header rows) = do
    coloredPutStrLn (name ++ "\n")
    prettyPrint2D   (newHeader : newRows)
    where
        newHeader = "ID" : header
        newRows   = [uncurry (:) p | p <- zip (map show [0..length rows - 1]) rows]
