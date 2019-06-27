{- |
Module      :  Miniframe.hs
Created     :  2019/06/26 03:29:55 PM
Description :  This is a description of the module
Copyright   :  (c) 2018 David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

License:
    The code is licensed under GNU General Public License v3.0.
    Please read the LICENSE file in this distribution for details

Description:
    This is an implementation of a data frame which comes with
    various common-for-data frames operations as well as some
    relational algebra goodness.

    Read more about relational algebra by following the link:
        https://en.wikipedia.org/wiki/Relational_algebra.
-}

------------------------------------------------------------------------------
--
-- TODO:
--
--     * Implement relational algebra operations
--
--     * Handle all the edge cases (might require a heavy use of Data.Maybe)
--
--     * Optimize functions
--
------------------------------------------------------------------------------

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

    -- Relational algebra
    , union                     -- Miniframe -> Miniframe -> Miniframe
    , (\\)                      -- Miniframe -> Miniframe -> Miniframe
    , intersect                 -- Miniframe -> Miniframe -> Miniframe
    , project                   -- [Name] -> Miniframe -> Miniframe
    , select                    -- (Header -> Row -> Bool) -> Miniframe -> Miniframe
    , rename                    -- Name -> Name -> Miniframe -> Miniframe
    , njoin                     -- Miniframe -> Miniframe -> Miniframe
    , thetaJoin                 -- (Header -> Row -> Bool) -> Miniframe -> Miniframe -> Miniframe
    , cartprod                  -- Miniframe -> Miniframe -> Miniframe

    -- Pretty-printing
    , printName                 -- Miniframe -> IO ()
    , printHeader               -- Miniframe -> IO ()
    , printRows                 -- Miniframe -> IO ()
    , printMf                   -- Miniframe -> IO ()
    ) where

import Data.List.Split
import Text.PrettyPrint.Boxes (printBox, hsep, left, vcat, text)
import ParseCSV
import Colors

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
        newHeader = header ++ [newColumnName]
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
--                         Relational algebra
--
-- The operators include union, difference, intersect, project, select,
-- rename,and join. These implementations could be improved in various
-- ways (more efficient, flexible etc). Operations like natural join,
-- theta join, equijoin, semijoin, antijoin, division, and cartesian
-- product will be implemented in the next few updates. Some of the
-- operations such as left and right outer joins, left and right inner
-- joins, full outer and full inner joins are yet to be implemented.
-------------------------------------------------------------------------------

-- | Union operation from relational algebra
union :: Miniframe -> Miniframe -> Miniframe
union (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = name ++ " union " ++ otherName
        newHeader = header
        newRows   = rows `List.union` otherRows

-- | Difference operation from relational algebra
(\\) :: Miniframe -> Miniframe -> Miniframe
(\\) (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = name ++ " difference " ++ otherName
        newHeader = header
        newRows   = rows List.\\ otherRows

-- | Intersect operation from relational algebra
intersect :: Miniframe -> Miniframe -> Miniframe
intersect (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = Miniframe newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header
        newRows   = rows `List.intersect` otherRows

-- Note that this won't work if the user does not pass the list of columns in the right order.
-- This is due to how `List.isSubsequenceOf` works. Will have to reimplement this operation so that
-- the order does not play any role in determining the output.

-- | Project operation from relational algebra
project :: [Name] -> Miniframe -> Miniframe
project columnNames miniframe@(Miniframe name header rows)
    | columnNames `List.isSubsequenceOf` header = Miniframe newName newHeader newRows
    | otherwise                                 = error "Header mismatch: put the columns in the right order!"
    where
        newName   = "Projected " ++ name
        newHeader = columnNames
        newRows   = List.transpose (map (columnByName miniframe) columnNames)

-- | Select operation from relational algebra
select :: (Header -> Row -> Bool) -> Miniframe -> Miniframe
select function (Miniframe name header rows) = Miniframe ("Selected " ++ name) header (filter (function header) rows)

-- | Rename operation from relational algebra
rename :: Name -> Name -> Miniframe -> Miniframe
rename oldColumnName newColumnName miniframe@(Miniframe name header rows)
    | oldColumnName `elem` header = Miniframe newName newHeader newRows
    | otherwise                   = miniframe
    where
        index     = Maybe.fromJust (List.elemIndex oldColumnName header)
        newName   = name
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header
        newRows   = rows


-- The following is a proto version that needs to be tested

-- | Natural join operation from relational algebra
njoin :: Miniframe -> Miniframe -> Miniframe
njoin miniframe@(Miniframe name header rows) otherMiniframe@(Miniframe otherName otherHeader otherRows)
    | null commonColumnNames  = error "No common column names"
    | columns                 /= otherColumns = error "No common columns"
    | otherwise               = Miniframe newName newHeader newRows
    where
        commonColumnNames = header `List.intersect` otherHeader
        columns           = map (columnByName miniframe) commonColumnNames
        otherColumns      = map (columnByName otherMiniframe) commonColumnNames
        ----
        newName           = name ++ " njoin " ++ otherName
        newHeader         = List.nub (header ++ otherHeader)
        newRows           = List.nub (rows ++ otherRows)

-- | Theta join operation from relational algebra
thetaJoin :: (Header -> Row -> Bool) -> Miniframe -> Miniframe -> Miniframe
thetaJoin function miniframe otherMiniframe = select function (njoin miniframe otherMiniframe)

-- | Cartesian product operation from relational algebra
cartprod :: Miniframe -> Miniframe -> Miniframe
cartprod (Miniframe name header rows) (Miniframe otherName otherHeader otherRows)
    | header List.\\ otherHeader /= header = error "Cannot perform cartesian product on duplicate column names"
    | otherwise                            = Miniframe newName newHeader newRows
    where
        newName   = name ++ " cartprod " ++ otherName
        newHeader = header ++ otherHeader
        newRows   = [x ++ y | x <- rows, y <- otherRows]

-------------------------------------------------------------------------------
--                   Printing and Pretty-printing
--
-- Currently, there are four printing tools provided with this package.
--
--         printName: Prints the name of the Miniframe
--         printName: Prints the header of the Miniframe
--         printRows: Prints the rows of the Miniframe
--         printMf:   Pretty-prints the Miniframe
--
-- 'project' and 'pretty-print' could be used in conjunction to display
-- chunks of the Miniframe, however, it would be a lot more convenient
-- to have separate implementations for such printing.
--
-- Another idea is to have two functions 'show' and 'prettyPrint'.
-- The first one would be the string representation of the Miniframe
-- with the latter one being the IO.
-------------------------------------------------------------------------------

-- | Print the name of the Miniframe
printName :: Miniframe -> IO ()
printName (Miniframe name _ _) = coloredPutStrLn name

-- | Print the header of the Miniframe
printHeader :: Miniframe -> IO ()
printHeader (Miniframe _ header _) = coloredPutStrLn $ concatMap (++"  ") (init header) ++ last header

-- | Print the rows of the Miniframe
printRows :: Miniframe -> IO ()
printRows (Miniframe _ _ rows) = coloredPrintBox $ hsep 2 left (map (vcat left . map text) (List.transpose rows))

-- | Print the miniframe
printMf :: Miniframe -> IO ()
printMf (Miniframe name header rows) = do
    coloredPutStrLn $ name ++ "\n"
    coloredPrintBox $ hsep 2 left (map (vcat left . map text) (List.transpose (header : rows)))

{-
This is an ugly function that did the work...
I stumbled upon the Boxes package now, so I do not need to use this mess.

-- | Print the table
printMf :: Miniframe -> IO ()
printMf (Miniframe name header rows) = do
    coloredPutStrLn (" " ++ replicate (length name + 2) '_' ++ "\n| " ++ name ++ " |\n " ++ replicate (length name + 2) '-' ++ "\n")
    coloredPutStrLn (List.intercalate "-+-" formattedDashes)
    coloredPutStrLn (List.intercalate " | " formattedHeader)
    coloredPutStrLn (List.intercalate "-+-" formattedDashes)
    mapM_ (coloredPutStrLn . List.intercalate " | ") rowsForPrettyPrint
    coloredPutStrLn (List.intercalate "-+-" formattedDashes)
    where
        -- Header stuff
        newHeader                 = "| ID" : header
        headerLengthList          = map length newHeader
        -- Rows with ID numbers
        rowsWithID                = [("| "++ show i) : rows !! i | i <- [0..length rows - 1]]
        -- Longest strings per column
        maxLengthStringsPerColumn = map (maximum . map length) (List.transpose rowsWithID)
        -- Comparing maximum string lengths across the header and columns for even spacing
        maxNumOfSpaces            = zipWith max headerLengthList maxLengthStringsPerColumn
        -- Dashes without pluses; we add 3 X columnsNum as `intercalate " | "` puts 3 characters, namely ' ', '|', and ' '
        formattedDashesHelper1    = [fst i ++ replicate (snd i - length (fst i)) '-' | i <- zip (map ((`replicate` '-') . length) newHeader) maxNumOfSpaces]
        formattedDashesHelper2    = init formattedDashesHelper1 ++ [last formattedDashesHelper1 ++ "-+"]
        formattedDashes           = ("+" ++ tail (head formattedDashesHelper2)) : tail formattedDashesHelper2
        -- Formatted header
        formattedHeaderHelper     = [fst i ++ replicate (snd i - length (fst i)) ' ' | i <- zip newHeader maxNumOfSpaces]
        formattedHeader           = init formattedHeaderHelper ++ [last formattedHeaderHelper ++ " |"]
        -- Formatted rows
        rowsForPrettyPrintHelper  = List.transpose [map (\n -> n ++ replicate (snd i - length n) ' ') (fst i) | i <- zip (List.transpose rowsWithID) maxNumOfSpaces]
        rowsForPrettyPrint        = List.transpose (init (List.transpose rowsForPrettyPrintHelper) ++ [map (++" |") (last (List.transpose rowsForPrettyPrintHelper))])
-}
