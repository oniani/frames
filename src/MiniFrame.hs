{- |
Module      :  MiniFrame.hs
Description :  Module implements MiniFrame data type and its core functionalities
Copyright   :  (c) 2019 David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  experimental
Portability :  portable

This is an implementation of a data frame which comes with
various common-for-data frames operations as well as some
relational algebra goodness.

You can read more about relational algebra by following the link:
    https://en.wikipedia.org/wiki/Relational_algebra.
-}

------------------------------------------------------------------------------
--
-- TODO:
--     * addRow function should throw an error if the number of items in
--       the row does not match the number of items in the MiniFrame row.
--
--     * Handle all the edge cases (might require a heavy use of Data.Maybe)
--
--     * Implement relational algebra operators
--
--     * Optimize functions
--
------------------------------------------------------------------------------

module MiniFrame
    (
    -- Data types
      MiniFrame (..)
    , Name
    , Header
    , Row
    , Column

    -- Construction
    , sample                    -- -> MiniFrame
    , fromNull                  -- -> MiniFrame
    , fromRows                  -- Name -> Header -> [Row] -> MiniFrame
    , fromColumns               -- Name -> Header -> [Column] -> MiniFrame
    , fromCSV                   -- String -> IO MiniFrame

    -- Retrieval
    , name                      -- MiniFrame -> Name
    , header                    -- MiniFrame -> Header
    , rows                      -- MiniFrame -> [Row]
    , columns                   -- MiniFrame -> [Column]
    , rowByID                   -- MiniFrame -> ID -> Row
    , columnByName              -- MiniFrame -> Name -> Column

    -- Dimensions
    , rowsNum                   -- MiniFrame -> Int
    , columnsNum                -- MiniFrame -> Int
    , entriesNum                -- MiniFrame -> Int

    -- Modification
    , renameMF                  -- Name -> MiniFrame -> MiniFrame
    , appendRow                 -- Row -> MiniFrame -> MiniFrame
    , prependRow                -- Row -> MiniFrame -> MiniFrame
    , appendColumn              -- Name -> Column -> MiniFrame -> MiniFrame
    , prependColumn             -- Name -> Column -> MiniFrame -> MiniFrame
    , insertRow                 -- ID -> Row -> MiniFrame -> MiniFrame
    , insertColumn              -- Name -> Name -> Column -> MiniFrame -> MiniFrame
    , removeRowByID             -- MiniFrame -> ID -> MiniFrame
    , removeColumnByName        -- MiniFrame -> Name -> MiniFrame

    -- Relational algebra
    , union                     -- MiniFrame -> MiniFrame -> MiniFrame
    , (\\)                      -- MiniFrame -> MiniFrame -> MiniFrame
    , intersect                 -- MiniFrame -> MiniFrame -> MiniFrame
    , project                   -- [Name] -> MiniFrame -> MiniFrame
    , select                    -- (Header -> Row -> Bool) -> MiniFrame -> MiniFrame
    , rename                    -- Name -> Name -> MiniFrame -> MiniFrame
    , njoin                     -- MiniFrame -> MiniFrame -> MiniFrame
    , thetaJoin                 -- (Header -> Row -> Bool) -> MiniFrame -> MiniFrame -> MiniFrame
    , cartprod                  -- MiniFrame -> MiniFrame -> MiniFrame

    -- Pretty-printing
    , printName                 -- MiniFrame -> IO ()
    , printHeader               -- MiniFrame -> IO ()
    , printRows                 -- MiniFrame -> IO ()
    , printMF                   -- MiniFrame -> IO ()
    ) where

import Data.List.Split

import ParseCSV
import Colors

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

type ID     = Int
type Name   = String
type Header = [String]
type Row    = [String]
type Column = [String]

data MiniFrame = MiniFrame
    { _name   :: Name           -- Name of the MiniFrame
    , _header :: Header         -- Header columns of the MiniFrame
    , _rows   :: [Row] }        -- Rows of the MiniFrame
    deriving (Eq, Show)

-------------------------------------------------------------------------------

-- | A sample MiniFrame
sample :: MiniFrame
sample = MiniFrame name header rows
    where
        name   = "MiniFrame"
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

-- | Built an empty MiniFrame (probably useless)
fromNull :: MiniFrame
fromNull = MiniFrame "" [] []

-- | Build a MiniFrame from rows
fromRows :: Name -> Header -> [Row] -> MiniFrame
fromRows = MiniFrame

-- | Build a MiniFrame from columns
fromColumns :: Name -> Header -> [Column] -> MiniFrame
fromColumns name header columns = MiniFrame name header (List.transpose columns)

-- | Build a MiniFrame from the CSV file
fromCSV :: String -> IO MiniFrame
fromCSV file
  | format /= ".csv" = error "Unknown file format!"
  | otherwise        = do csvData <- readCSV file
                          let header = head csvData
                          let rows   = tail csvData
                          return (MiniFrame "MiniFrame" header rows)
    where
        format = map Char.toLower $ drop (length file - 4) file

-------------------------------------------------------------------------------
-- Retrieval
-------------------------------------------------------------------------------

-- | Get the name
name :: MiniFrame -> Name
name (MiniFrame name _ _ ) = name

-- | Get the header
header :: MiniFrame -> Header
header (MiniFrame _ header _ ) = header

-- | Get the rows
rows :: MiniFrame -> [Row]
rows (MiniFrame _ _ rows ) = rows

-- | Get the columns
columns :: MiniFrame -> [Column]
columns (MiniFrame _ _ rows) = List.transpose rows

-- | Get the first entry
hd :: MiniFrame -> Row
hd (MiniFrame _ _ rows) = head rows

-- | Get the last entry
tl :: MiniFrame -> Row
tl (MiniFrame _ _ rows) = last rows

-- | Get a row by ID
rowByID :: MiniFrame -> ID -> Row
rowByID (MiniFrame _ _ rows) id
    | id < 0 || id >= length rows = error "Index out of bounds"
    | otherwise                   = rows !! id

-- | Get a column by name
columnByName :: MiniFrame -> Name -> Column
columnByName (MiniFrame _ header rows) columnName
    | columnName `elem` header = List.transpose rows !! index
    | otherwise                = error "Unknown column name"
    where
        index = Maybe.fromJust (List.elemIndex columnName header)

-------------------------------------------------------------------------------
-- Dimensions
-------------------------------------------------------------------------------

-- | Get the number of rows
rowsNum :: MiniFrame -> Int
rowsNum (MiniFrame _ _ rows) = length rows

-- | Get the number of columns
columnsNum :: MiniFrame -> Int
columnsNum (MiniFrame _ _ rows) = length (List.transpose rows)

-- | Get the number of entries
entriesNum :: MiniFrame -> Int
entriesNum mf = rowsNum mf * columnsNum mf

-------------------------------------------------------------------------------
-- Addition
-------------------------------------------------------------------------------

-- | Rename the MiniFrame
renameMF :: Name -> MiniFrame -> MiniFrame
renameMF newName (MiniFrame name header rows) = MiniFrame newName header rows

-- | Add a row to the end of the MiniFrame
appendRow :: Row -> MiniFrame -> MiniFrame
appendRow newRow (MiniFrame name header rows) = MiniFrame name header (rows ++ [newRow])

-- | Add a row to the beginning of the MiniFrame
prependRow :: Row -> MiniFrame -> MiniFrame
prependRow newRow (MiniFrame name header rows) = MiniFrame name header (newRow : rows)

-- | Add a column to the end of the MiniFrame
appendColumn :: Name -> Column -> MiniFrame -> MiniFrame
appendColumn newColumnName newColumn (MiniFrame name header rows) = MiniFrame name newHeader newRows
    where
        newHeader = header ++ [newColumnName]
        newRows   = List.transpose (List.transpose rows ++ [newColumn])

-- | Add a column to the beginning of the MiniFrame
prependColumn :: Name -> Column -> MiniFrame -> MiniFrame
prependColumn newColumnName newColumn (MiniFrame name header rows) = MiniFrame name newHeader newRows
    where
        newHeader = header ++ [newColumnName]
        newRows   = List.transpose (newColumn : List.transpose rows)

-- | Insert a row at the given ID
insertRow :: ID -> Row -> MiniFrame -> MiniFrame
insertRow id newRow (MiniFrame name header rows) = MiniFrame name header newRows
    where
        splitID = splitAt id rows
        newRows = fst splitID ++ [newRow] ++ snd splitID

-- | Insert a column at the given index (index starts from 0)
insertColumn :: ID -> Name -> Row -> MiniFrame -> MiniFrame
insertColumn index newColumnName newColumn (MiniFrame name header rows)
    | length newColumn /= length rows = error "Wrong column size!"
    | otherwise = MiniFrame name newHeader newRows
        where
            splitIndex  = splitAt index (List.transpose rows)
            newRows     = List.transpose (fst splitIndex ++ [newColumn] ++ snd splitIndex)
            splitHIndex = splitAt index header
            newHeader   = fst splitHIndex ++ [newColumnName] ++ snd splitHIndex

-------------------------------------------------------------------------------
-- Removal
-------------------------------------------------------------------------------

-- | Remove a row by ID
removeRowByID :: MiniFrame -> ID -> MiniFrame
removeRowByID miniframe@(MiniFrame name header rows) id
    | id < 0 || id >= length rows = miniframe
    | otherwise                   = MiniFrame name header (take id rows ++ drop (id + 1) rows)

-- | Remove a column by name
removeColumnByName :: MiniFrame -> Name -> MiniFrame
removeColumnByName miniframe@(MiniFrame name header rows) columnName
    | columnName `elem` header = MiniFrame name newHeader newRows
    | otherwise                = miniframe
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
union :: MiniFrame -> MiniFrame -> MiniFrame
union (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " union " ++ otherName
        newHeader = header
        newRows   = rows `List.union` otherRows

-- | Difference operation from relational algebra
(\\) :: MiniFrame -> MiniFrame -> MiniFrame
(\\) (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " difference " ++ otherName
        newHeader = header
        newRows   = rows List.\\ otherRows

-- | Intersect operation from relational algebra
intersect :: MiniFrame -> MiniFrame -> MiniFrame
intersect (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header /= otherHeader = error "Header mismatch"
    | otherwise             = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " intersect " ++ otherName
        newHeader = header
        newRows   = rows `List.intersect` otherRows

-- Note that this won't work if the user does not pass the list of columns in the right order.
-- This is due to how `List.isSubsequenceOf` works. Will have to reimplement this operation so that
-- the order does not play any role in determining the output.

-- | Project operation from relational algebra
project :: [Name] -> MiniFrame -> MiniFrame
project columnNames miniframe@(MiniFrame name header rows)
    | columnNames `List.isSubsequenceOf` header = MiniFrame newName newHeader newRows
    | otherwise                                 = error "Header mismatch: put the columns in the right order!"
    where
        newName   = "Projected " ++ name
        newHeader = columnNames
        newRows   = List.transpose (map (columnByName miniframe) columnNames)

-- | Select operation from relational algebra
select :: (Header -> Row -> Bool) -> MiniFrame -> MiniFrame
select function (MiniFrame name header rows) = MiniFrame ("Selected " ++ name) header (filter (function header) rows)

-- | Rename operation from relational algebra
rename :: Name -> Name -> MiniFrame -> MiniFrame
rename oldColumnName newColumnName miniframe@(MiniFrame name header rows)
    | oldColumnName `elem` header = MiniFrame newName newHeader newRows
    | otherwise                   = miniframe
    where
        index     = Maybe.fromJust (List.elemIndex oldColumnName header)
        newName   = name
        newHeader = take index header ++ [newColumnName] ++ drop (index + 1) header
        newRows   = rows


-- The following is a proto version that needs to be tested

-- | Natural join operation from relational algebra
njoin :: MiniFrame -> MiniFrame -> MiniFrame
njoin miniframe@(MiniFrame name header rows) otherMiniframe@(MiniFrame otherName otherHeader otherRows)
    | null commonColumnNames  = error "No common column names"
    | columns                 /= otherColumns = error "No common columns"
    | otherwise               = MiniFrame newName newHeader newRows
    where
        commonColumnNames = header `List.intersect` otherHeader
        columns           = map (columnByName miniframe) commonColumnNames
        otherColumns      = map (columnByName otherMiniframe) commonColumnNames
        ----
        newName           = name ++ " njoin " ++ otherName
        newHeader         = List.nub (header ++ otherHeader)
        newRows           = List.nub (rows ++ otherRows)

-- | Theta join operation from relational algebra
thetaJoin :: (Header -> Row -> Bool) -> MiniFrame -> MiniFrame -> MiniFrame
thetaJoin function miniframe otherMiniframe = select function (njoin miniframe otherMiniframe)

-- | Cartesian product operation from relational algebra
cartprod :: MiniFrame -> MiniFrame -> MiniFrame
cartprod (MiniFrame name header rows) (MiniFrame otherName otherHeader otherRows)
    | header List.\\ otherHeader /= header = error "Cannot perform cartesian product on duplicate column names"
    | otherwise                            = MiniFrame newName newHeader newRows
    where
        newName   = name ++ " cartprod " ++ otherName
        newHeader = header ++ otherHeader
        newRows   = [x ++ y | x <- rows, y <- otherRows]

-------------------------------------------------------------------------------
--                   Printing and Pretty-printing
--
-- Currently, there are four printing tools provided with this package.
--
--         printName: Prints the name of the MiniFrame
--         printName: Prints the header of the MiniFrame
--         printRows: Prints the rows of the MiniFrame
--         printMF:   Pretty-prints the MiniFrame
--
-- 'project' and 'pretty-print' could be used in conjunction to display
-- chunks of the MiniFrame, however, it would be a lot more convenient
-- to have separate implementations for such printing.
--
-- Another idea is to have two functions 'show' and 'prettyPrint'.
-- The first one would be the string representation of the MiniFrame
-- with the latter one being the IO.
-------------------------------------------------------------------------------

-- | Print the name of the table
printName :: MiniFrame -> IO ()
printName (MiniFrame name _ _) = print name

-- | Print the header of the table
printHeader :: MiniFrame -> IO ()
printHeader (MiniFrame _ header _) = print header

-- | Print the rows of the table
printRows :: MiniFrame -> IO ()
printRows (MiniFrame _ _ rows) = mapM_ print rows

-- | Print the table
printMF :: MiniFrame -> IO ()
printMF (MiniFrame name header rows) = do
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
